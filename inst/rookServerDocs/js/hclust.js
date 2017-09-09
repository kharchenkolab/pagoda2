"use strict";

/*
 * Filename: hclust.js
 * This file is derived from hclust.js
 * URL: https://github.com/harthur/clusterfck/blob/master/lib/hcluster.js
 * but modified so that items have labels and
 * a correlation distance metric is available
 */

var distances = {
    corrdist: function(a, b) {
    	// NOTE: using math.js

    	var a_mean = math.mean(a);
    	var b_mean = math.mean(b);

    	var a_std = math.std(a, 'uncorrected');
    	var b_std = math.std(b, 'uncorrected');

    	var a_std_b_std = (a_std * b_std);
    	if (a_std_b_std == 0) return 1; // If one of the two has std == 0

    	var covariance = math.mean(
    	    math.dotMultiply(math.subtract(a, a_mean),
    			     math.subtract(b, b_mean))
    	);
    	var correlation = covariance / a_std_b_std;

    	return 1 - (correlation + 1) / 2
    },

    euclidean: function(v1, v2) {
	var total = 0;
	for (var i = 0; i < v1.length; i++) {
            total += Math.pow(v2[i] - v1[i], 2);
	}
	return Math.sqrt(total);
    },
    manhattan: function(v1, v2) {
	var total = 0;
	for (var i = 0; i < v1.length ; i++) {
            total += Math.abs(v2[i] - v1[i]);
	}
	return total;
    },
    max: function(v1, v2) {
	var max = 0;
	for (var i = 0; i < v1.length; i++) {
            max = Math.max(max , Math.abs(v2[i] - v1[i]));
	}
	return max;
    }
};




var HierarchicalClustering = function(distance, linkage, threshold) {
    this.distance = distance;
    this.linkage = linkage;
    this.threshold = threshold == undefined ? Infinity : threshold;
}

HierarchicalClustering.prototype = {
    cluster : function(items, labels, snapshotPeriod, snapshotCb) {
	this.clusters = [];
	this.dists = [];  // distances between each pair of clusters
	this.mins = []; // closest cluster for each cluster
	this.index = []; // keep a hash of all clusters by key

	for (var i = 0; i < items.length; i++) {
            var cluster = {
		value: items[i],
		key: i,
		index: i,
		size: 1,
		label: labels[i]
            };
            this.clusters[i] = cluster;
            this.index[i] = cluster;
            this.dists[i] = [];
            this.mins[i] = 0;
	}

	for (var i = 0; i < this.clusters.length; i++) {
            for (var j = 0; j <= i; j++) {
		var dist = (i == j) ? Infinity :
		    this.distance(this.clusters[i].value, this.clusters[j].value);
		this.dists[i][j] = dist;
		this.dists[j][i] = dist;

		if (dist < this.dists[i][this.mins[i]]) {
		    this.mins[i] = j;
		}
            }
	}

	var merged = this.mergeClosest();
	var i = 0;
	while (merged) {
            if (snapshotCb && (i++ % snapshotPeriod) == 0) {
		snapshotCb(this.clusters);
            }
            merged = this.mergeClosest();
	}

	this.clusters.forEach(function(cluster) {
            // clean up metadata used for clustering
            delete cluster.key;
            delete cluster.index;
	});

	return this.clusters;
    },

    mergeClosest: function() {
	// find two closest clusters from cached mins
	var minKey = 0, min = Infinity;
	for (var i = 0; i < this.clusters.length; i++) {
            var key = this.clusters[i].key,
            dist = this.dists[key][this.mins[key]];
            if (dist < min) {
		minKey = key;
		min = dist;
            }
	}
	if (min >= this.threshold) {
            return false;
	}

	var c1 = this.index[minKey],
        c2 = this.index[this.mins[minKey]];

	// merge two closest clusters
	var merged = {
            left: c1,
            right: c2,
            key: c1.key,
            size: c1.size + c2.size
	};

	this.clusters[c1.index] = merged;
	this.clusters.splice(c2.index, 1);
	this.index[c1.key] = merged;

	// update distances with new merged cluster
	for (var i = 0; i < this.clusters.length; i++) {
            var ci = this.clusters[i];
            var dist;
            if (c1.key == ci.key) {
		dist = Infinity;
            }
            else if (this.linkage == "single") {
		dist = this.dists[c1.key][ci.key];
		if (this.dists[c1.key][ci.key] > this.dists[c2.key][ci.key]) {
		    dist = this.dists[c2.key][ci.key];
		}
            }
            else if (this.linkage == "complete") {
		dist = this.dists[c1.key][ci.key];
		if (this.dists[c1.key][ci.key] < this.dists[c2.key][ci.key]) {
		    dist = this.dists[c2.key][ci.key];
		}
            }
            else if (this.linkage == "average") {
		dist = (this.dists[c1.key][ci.key] * c1.size
			+ this.dists[c2.key][ci.key] * c2.size) / (c1.size + c2.size);
            }
            else {
		dist = this.distance(ci.value, c1.value);
            }

            this.dists[c1.key][ci.key] = this.dists[ci.key][c1.key] = dist;
	}


	// update cached mins
	for (var i = 0; i < this.clusters.length; i++) {
            var key1 = this.clusters[i].key;
            if (this.mins[key1] == c1.key || this.mins[key1] == c2.key) {
		var min = key1;
		for (var j = 0; j < this.clusters.length; j++) {
		    var key2 = this.clusters[j].key;
		    if (this.dists[key1][key2] < this.dists[key1][min]) {
			min = key2;
		    }
		}
		this.mins[key1] = min;
            }
            this.clusters[i].index = i;
	}

	// clean up metadata used for clustering
	delete c1.key; delete c2.key;
	delete c1.index; delete c2.index;

	return true;
    }
}

var hcluster = function(items, labels, distance, linkage, threshold, snapshot, snapshotCallback) {
    distance = distance || "euclidean";
    linkage = linkage || "average";
    labels = labels || [];

    if (typeof distance == "string") {
	distance = distances[distance];
    }
    var clusters = (new HierarchicalClustering(distance, linkage, threshold))
        .cluster(items, labels, snapshot, snapshotCallback);

    if (threshold === undefined) {
	return clusters[0]; // all clustered into one
    }
    return clusters;
}


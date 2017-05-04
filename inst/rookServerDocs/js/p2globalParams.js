
/**
 * @namespace
 * This is an namespace that keeps all the startup and on going
 * parameters for this pagoda app.
 */
var p2globalParams = {
/* --- PARAMS START --- */
    generalParams: {
	applicationName: "Development Pagoda Application",
	applicationGenerated: "Saturday, January 28th 2017",
	// etc...
    },

    misc: {
	jaxGeneQueryFormatString: '<a href="http://www.informatics.jax.org/searchtool/Search.do?query={0}" target="_blank">{1}</a>'
    },

    embedding: {
	defaultEmbedding: {"reduction": "PCA", "embedding": "tSNE"},
	defaultPlotPointSize: 4,
	defaultAlpha: 0.4,
	defaultBorder: true,
	defaultBorderColor: 'black',
	defaultBorderWidth: 0.25
    },

   // Heatmap and dendrogram viewer plotting parameters
    dendrogramHeatmapViewer: {
	paddingLeft: 10,
	paddingRight: 100, // Space for the labels

    },

    aspectViewer: {
      defaultPaletteName: 'tol-dv',
      defaultPaletteLevels: 20,
    },

    heatmapViewer: {
	fontBaseSize:  20,
	defaultPaletteLevels: 15,
	defaultPaletteName: 'default1',
	defaultRowReordering: true,

	// From palette.js -- many more options
	// TODO: Extend to support parametrisable palettes and have different display
	// and internal names
	availablePalettes: {
	    'default1': {
		name: 'default1',
		displayName: 'Pagoda default',
		maxColors: 11,
		minColors: 11,
		generate: 'fixed-values',
		colorValues: [ "#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
			       "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061" ]
	    },
	    'default2': {
		name: 'default2',
		displayName: 'Pagoda default 2',
		maxColors: 3,
		minColors: 3,
		generate: 'fixed-values',
		colorValues: [ "#0000FF", "#FFFFFF", "#FF0000" ]
	    },
	    'tol-dv': {
		name: 'tol-dv',
		displayName: 'Blue-White-Red',
		maxColors: 100,
		minColors: 2,
		generate: 'palette.js-standard'
	    },
	    'cb-RdYlBu': {
		name: 'cb-RdYlBu',
		displayName: 'Red-Yellow-Blue',
		maxColors: 11,
		minColors: 2,
		generate: 'palette.js-standard'
	    },
	    'cb-Blues': {
		name: 'cb-Blues',
		displayName: 'Blues',
		maxColors: 8,
		minColors: 2,
		generate: 'palette.js-standard'
	    },
	    "tol-rainbow": {
		name: "tol-rainbow",
		displayName: 'Rainbow',
		maxColors: 8,
		minColors: 2,
		generate: 'palette.js-standard'
		},
	    "cb-Spectral": {
		name: "cb-Spectral",
		displayName: 'Spectral',
		maxColors: 8,
		minColors: 2,
		generate: 'palette.js-standard'
	    }
	}
    },

    // Dendrogram parameters
    dendrogram: {
	currentType: "dummy",

	// These are the selection colours that will be used
        // to hightlight dendrogram selected cells in the embedding
	selectionColors: {
	    selectedMain: "red",
	    selectedMainRGB: [255, 0,0],
	    selectedAlt: "green",
	    selectedAltRGB: [0,255, 0],
	    deselected: "grey",
	    deselectedRGB: [127,127,127]
	},

	// A vertical scale factor for the dendrogram plot
	scaleFactor: 0.95
    }
/* --- PARAMS END --- */
};

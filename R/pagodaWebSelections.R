# File: pagodaWebSelection
# Author: Nikolas Barkas
# Date: August 2017
# Description: A collection of functions for working with p2 selections

#' @title reads a pagoda2 web app exported cell selection file
#' @description reads a cell selection file exported by pagoda2 web interface as a list
#' of list objects that contain the name of the selection, the color (as a hex string) and the
#' identifiers of the individual cells
#' @param filepath the path of the file load
#' @export readPagoda2SelectionFile
readPagoda2SelectionFile <- function(filepath) {
  returnList <- list();

  con <- file(filepath, "r");
  while (TRUE) {
    suppressWarnings(line <- readLines(con, n = 1))
    if ( length(line) == 0 ) {
      break
    }

    fields <- unlist(strsplit(line, split=',', fixed=T));

    name <- make.names(fields[2]);
    color <- fields[1];
    cells <- fields[-c(1:2)];
    returnList[[name]] <- list(name=fields[2], color = color, cells = cells);
  }
  close(con)

  invisible(returnList)
}

#' @title writes a pagoda2 selection object as a p2 pagoda2 selection files
#' @description writes a pagoda2 selection object as a p2 selection file that be be
#' loaded to the web interfact
#' @param sel pagoda2 selection object
#' @param filepath name of file to write to
#' @export writePagoda2SelectionFile
writePagoda2SelectionFile <- function(sel, filepath) {
  fileConn <- file(filepath);
  lines <- c();
  for (l in names(sel)) {
    cells <- sel[[l]]$cells
    cellsString <- paste0(cells,collapse=',');
    ln <- paste(sel[[l]]$color,as.character(l),cellsString,sep=',');
    lines <- c(lines,ln)
  }
  writeLines(lines, con=fileConn);
  close(fileConn);
}

#' @title writes a list of genes as a gene selection that can be loaded in the web interface
#' @description writes a list of genes as a gene selection that can be loaded in the web interfact
#' @param name the name of the selection
#' @param genes a string vector of the gene names
#' @param filename the filename to save to
#' @export writeGenesAsPagoda2Selection
writeGenesAsPagoda2Selection <- function(name, genes, filename) {
  con <- file(filename, 'w')
  cat(name, file=con)
  cat(',',file=con)
  cat(paste(genes, collapse=','),file=con)
  cat('\n', file=con)
  close(con)
}

#' @title returns a list vector with the number of multiclassified cells
#' @description returns a list vector with the number of cells that are
#' present in more than one selections in the provided p2 selection object
#' @param sel a pagoda2 selection as genereated by readPagoda2SelectionFile
#' @export calcMulticlassified
calcMulticlassified <- function(sel) {
  selectionCellsFlat <- unname(unlist(sapply(sel, function(x) x$cells)))
  multiClassified <- selectionCellsFlat[duplicated(selectionCellsFlat)]
  sort(sapply(sel, function(x) { sum(x$cells %in% multiClassified) / length(x$cells) }))
}

#' @title returns a factor of cell membership from a p2 selection
#' @description returns a factor of cell membership from a p2 selection object
#' the factor only includes cells present in the selection. If the selection
#' contains multiclassified cells an error is raised
#' @export factorFromP2Selection
factorFromP2Selection <- function (sel, use.internal.name=F)
{
  if (!all(calcMulticlassified(sel) == 0)) {
    stop("The selections provided are not mutually exclusive")
  }
  x <- mapply(function(x,n) {
    if (length(x$cells) > 0) {
      data.frame(cellid = x$cells, label = c(x$name), internal.name = c(n))
    }
  }, sel, names(sel), SIMPLIFY = FALSE)
  d <- do.call(rbind, x)
  if (use.internal.name) {
    f <- as.factor(d$internal.name)
  } else {
    f <- as.factor(d$label)
  }
  names(f) <- d$cellid
  f
}

#' @title get the colors for each selection from a p2 selection object
#' @description retrieves the colors of each selection from a p2 selection object as
#' a names vector of strings
#' @return a named vector of hex colours
#' @export getColorsFromP2Selection
getColorsFromP2Selection <- function(sel) {
  unlist(lapply(sel, function(x) { paste0('#',x$color); } ))
}

#' @title converts a factor to a p2 selection object
#' @description converts a names factor to a p2 selection object
#' if colors are provided it assigns those, otherwise uses a rainbow palette
#' @param col names vector of colors
#' @return a p2 selection object (list)
#' @export factorToP2selection
factorToP2selection <- function(cl,col=NULL) {
  if(!is.factor(cl)) {
    stop('cl is not a factor');
  }
  # If no colors are provided generate some random ones
  if(is.null(col)) {
    col=substr(rainbow(nlevels(cl)),2,7); # Rainbow w/o alpha and hash
    names(col) <- levels(cl);
  }
  ns <- list();
  for (l in levels(cl)) {
    ns[[l]] <- list(
      name = l,
      cells = names(cl)[which(cl == l)],
      color=col[l]
    )
  }
  invisible(ns)
}

#' @title remove cells that are present in more than one selection from all selections
#' @description remove cells that are present in more than one selection from all the
#' selections they are in
#' @param selection a pagoda2 selections list
#' @return a new list with the duplicated cells removed
#' @export removeSelectionOverlaps
removeSelectionOverlaps <- function(selections) {
  selectionsCellsFlat <- unname(unlist(sapply(selections, function(x) x$cells)))
  multiClassified <- selectionsCellsFlat[duplicated(selectionsCellsFlat)]

  lapply(selections, function(x) {
    r <- list();
    r$name = x$name;
    r$color = x$color;
    r$cells = x$cells[!x$cells %in% multiClassified];
    r;
  });
}

#' @title get the number of cells per selection group
#' @description get the number of cells in each selection group
#' @param selection a pagoda2 selection list
#' @return a named vector of cell numbers in each grous
#' @export cellsPerSelectionGroup
cellsPerSelectionGroup <- function(selection) {
  unlist(lapply(selection, function(x) length(x$cells)))
}

#' Validates a pagoda2 selection object
#' @description validates a pagoda2 selection object
#' @param selections the pagoda2 selection object to be validated
#' @return a logical value indicating if the object is valid
#' @export validateSelectionsObject
validateSelectionsObject <- function(selections) {
  t <- lapply(selections, function(x) {
    isvalidentry <- TRUE;
    if (!is.character(x$name)) {
      isvalidentry <- FALSE;
    }
    if (!is.character(x$color)) {
      isvalidentry <- FALSE;
    }
    if (!is.character(x$cells)) {
      isvalidentry <- FALSE;
    }
    isvalidentry;
  })

  all(unlist(t))
}

#' Given a clustering vector and a set of selections assign names to the clusters
#' @description This function will use a set of pagoda2 cell seletcion to identify
#' the clusters in a a named factor. It is meant to be used to import user defined annotations
#' that are defined as selections into a more formal categorization of cells that are defined by cluster.
#' To help with this the function allows a percent of cells to have been classified in the selections into
#' multiple groups, something which may be the result of the users making wrong selections. The percent of
#' cells allows to be multiselected in any given group is defined by multiClassCutoff. Furthermore
#' the method will assign each cluster to a selection only if the most popular cluster to the next most popular
#' exceed the ambiguous.ratio in terms of cell numbers. If a cluster does not satisfy this condtiion it is not
#' assigned.
#' @param clustering a named factor of clusters, where every entry is a cell
#' @param selections a pagoda2 selection object
#' @param multiClasscutoff percent of cells in any one cluster that can be multiassigned
#' @param ambiguour.ratio the ratio of first and second cell numbers for any cluster to produce a valid clustering
#' @return a data.frame with two colums, one for cluster and one for selections, each cluster appears only once
#' @export getClusterLabelsFromSelection
getClusterLabelsFromSelection <- function(clustering, selections, multiClassCutoff = 0.3, ambiguous.ratio = 0.5) {
  if (!is.factor(clustering)) {
    stop('clustering is not a factor');
  }

  if (!validateSelectionsObject(selections)) {
    stop('selections is not a valid selection object');
  }

  multiClass <- calcMulticlassified(selections)

  # Stop if the any of the selections exceed the specified multiclass cutoff
  if(!all(multiClass < multiClassCutoff)) {
    msg <- paste0('The following selections have a very high number of multiclassified cells: ',
                  paste(names(multiClass)[!multiClass < multiClassCutoff], collapse = ', '), 
                  '. Please reduce the overlaps and try again. You can use calcMulticlassified() to see more details.')
    stop(msg)
  }

  # Clean selections of overlaps
  sel.clean <- removeSelectionOverlaps(selections)
  sel.clean.vector <- factorFromP2Selection(sel.clean)

  shared.names <- intersect(names(sel.clean.vector), names(clustering))

  confusion.table <- table(data.frame(sel.clean.vector[shared.names],clustering[shared.names]))


  tmp1 <- plyr::adply(.data = confusion.table, .margins = 2, .fun =  function(x) {
    rv <- NA
    x.sort <- sort(x, decreasing=T)
    if((x.sort[1] * ambiguous.ratio) >= x.sort[2]) {
      rv <- names(x.sort)[1]
    }
    rv
  })

  labels <- tmp1[,2]
  names(labels) <- tmp1[,1]

  colnames(tmp1) <- c('cluster', 'selection')
  tmp1

}

#' Given a cell clustering (partitioning) and a set of user provided selections
#' generate a cleaned up annotation of cluster groups that can be used for classification
#' @param clustering a factor that provides the clustering
#' @param selection a p2 selection object that provided by the web interfact user
#' @return a named factor that can be used for classification
#' @export generateClassificationAnnotation
generateClassificationAnnotation <- function(clustering, selections) {
  clAnnotation <- getClusterLabelsFromSelection(clustering, selections);
  rownames(clAnnotation) <- clAnnotation$cluster

  r <- as.factor(clAnnotation[as.character(clustering),]$selection)
  names(r) <- names(clustering)

  r
}

#' Returns all the cells that are in the designated selections
#' @description Given a pagoda2 selections object and the names of some selections
#' in it returns the names of the cells that are in these selections removed any duplicates
#' @param p2selections a p2 selections object
#' @param selectionNames the names of some selections in th p2 object
#' @return a character vector of cell names
#' @export getCellsInSelections
getCellsInSelections <- function(p2selections, selectionNames) {
  if(!is.character(selectionNames)) {
    stop('selectionNames needs to be a character vector of cell names');
  }

  if(!validateSelectionsObject(p2selections)) {
    stop('p2selections is not a valid p2 selection object');
  }

  if(any(!selectionNames %in% names(p2selections))) {
    stop('Some selection names were not found in the pagoda2 selections object');
  }

  cells <- unique(unname(unlist(lapply(p2selections[selectionNames], function(x) x$cells))))

  cells
}

#' Get a dataframe and plot summarising overlaps between selection of a pagoda2 selection object
#' @description Get a dataframe and plot summarising overlaps between selection of a pagoda2 selection object
#' ignore self overlaps
#' @param sel a pagoda2 selection object
#' @return a list that contains a ggplot2 object and a datatable with the overlaps data
#' @export plotSelectionOverlaps
plotSelectionOverlaps <- function(sel) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.", call. = FALSE)
  }
  
  n1s = c()
  n2s = c();
  overlaps = c();
  for (n1 in names(sel)) {
    for (n2 in names(sel)) {
      if (n1 != n2) {
        overlapC = length(which(sel[[n1]]$cells %in% sel[[n2]]$cells))
      } else {
        overlapC = 0;
      }
      n1s <- c(n1s, n1);
      n2s <- c(n2s, n2);
      overlaps = c(overlaps,overlapC)
    }
  }
  res <- data.frame(cbind(n1s, n2s, overlaps),stringsAsFactors=F)
  res$overlaps <- as.numeric(res$overlaps)

  p <- ggplot2::ggplot(res, ggplot2::aes(n1s, n2s)) + ggplot2::geom_tile(ggplot2::aes(fill=log10(overlaps))) +  
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::geom_text(ggplot2::aes(label=(overlaps))) + 
    ggplot2::scale_fill_gradient(low = "yellow", high = "red")

  invisible(list(results=res, plot=p))
}

#' Plot multiclassified cells per selection as a percent barplot
#' @description Plot multiclassified cells per selection as a percent barplot
#' @param sel pagoda2 selection object
#' @return ggplot2 object
#' @export plotMulticlassified
plotMulticlassified <- function(sel) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.", call. = FALSE)
  }
  
  multiclassified <- calcMulticlassified(sel)
  tmp1 <- as.data.frame(multiclassified)
  tmp1$lab <- rownames(tmp1)

  p <- ggplot2::ggplot(tmp1, ggplot2::aes(x=lab, y= multiclassified)) + ggplot2::geom_bar(stat='identity') +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::scale_y_continuous(name='% multiclassified') +
    ggplot2::scale_x_discrete(name='Selection Label')

  p
}

#' Compare two different clusterings provided as factors by plotting a normalised heatmap
#' @param cl1 clustering 1, a named factor
#' @param cl2 clustering 2, a named factor
#' @param filename an optional filename to save the plot instead of displaying it, will be passed to pheatmap
#' @return invisible summary table that gets plotted
#' @export compareClusterings
compareClusterings <- function(cl1, cl2, filename = NA) {
  if (!requireNamespace("pheatmap", quietly = TRUE)) {
    stop("Package \"pheatmap\" needed for this function to work. Please install it.", call. = FALSE)
  }

  n1 <- names(cl1);
  n2 <- names(cl2);

  if(!all(n1 %in% n2) || !all(n2 %in% n1)) {
    warning('Clusterings do not completely overlap!');
    ns <- intersect(n1,n2)
  } else {
    ns <- n1
  }

  tbl  <- table(data.frame(cl1[ns],cl2[ns]))
  tblNorm <- sweep(tbl, 2, colSums(tbl), FUN=`/`)

  pheatmap::pheatmap(tblNorm, file=filename)

  invisible(tblNorm)
}

#' Perform differential expression on a p2 object given a
#' set of web selections and two groups to compare
#' @param p2 a pagoda2 object
#' @param sel a web selection read with readPagoda2SelectionFile()
#' @param group1name the name of the first selection
#' @param group2name the names of the second selection
diffExprOnP2FromWebSelection <- function(p2, sel, group1name, group2name) {
  # Get cell names
  grp1cells <- sel[[group1name]]$cells
  grp2cells <- sel[[group2name]]$cells

  # Make suitable factor
  t1 <- rep(NA, length(rownames(p2$counts)))
  names(t1) <- rownames(p2$counts)
  t1[grp1cells] <- c(group1name)
  t1[grp2cells] <- c(group2name)

  p2$getDifferentialGenes(groups=t1)
}

#' Perform differential expression on a p2 object given a
#' set of web selections and one group to compare against everything else
#' @param p2 a pagoda2 object
#' @param sel a web selection read with readPagoda2SelectionFile()
#' @param groupname name of group to compare to everything else
diffExprOnP2FromWebSelectionOneGroup <- function(p2, sel, groupname) {
  grp1cells <- sel[[groupname]]$cells
  t1 <- rep('other', length(rownames(p2$counts)))
  names(t1) <- rownames(p2$counts)
  t1[grp1cells] <- c(groupname)
  p2$getDifferentialGenes(groups=t1)
}

#' Get a mapping form internal to external names for the specified selection object
#' @param x p2 selection object
#' @export getIntExtNamesP2Selection
getIntExtNamesP2Selection <- function(x) unlist(lapply(x,function(y) {y$name}))

#' Plot a specified pagoda2 embedding according to the selection object
#' @description Plots the specified pagoda2 embedding according to the specified selection object
#' @param emb An embedding from a pagoda2 object
#' @param sel a pagoda2 web selection object
#' @param show.unlabelled display the plots without any associated selection as grey
#' @param show.labels display the labels at the center of each cluster
#' @return a ggplot2 object
#' @export plotEmbeddingColorByP2Selection
plotEmbeddingColorByP2Selection <- function(emb, sel, show.unlabelled = TRUE, show.labels = TRUE, label.size = 3, point.size=2, show.guides = T, alpha = 0.7, show.axis =T )  {
  # Return the plot variable p
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.", call. = FALSE)
  }
  
  p <- NULL

  colsorig <- pagoda2:::getColorsFromP2Selection(sel)
  f <- factorFromP2Selection(sel, use.internal.name= T)


  dftmp <- data.frame(emb)
  dftmp$selname <- as.character(f[rownames(dftmp)])

  if (show.unlabelled) {
    dftmp$selname[is.na(dftmp$selname)] <- 'no label'
    colsorig <- c(colsorig, 'no label' = 'grey50')
  }

  guidesVar <- NULL
  if (!show.guides) {
    guidesVar <- ggplot2::theme(legend.position = "none")
  }

  themeVar <- ggplot2::theme_bw()
  if (!show.axis) {
    themeVar <- ggplot2::theme_void()
  }

  if (show.labels) {
    clcnt <- aggregate(dftmp[,c(1:2)], by= list(sel=dftmp$selname), FUN=mean)
    clcnt <- clcnt[clcnt$sel != 'no label',]
    # Display external names
    clcnt$sel <- as.character(getIntExtNamesP2Selection(sel)[clcnt$sel])

    p <- ggplot2::ggplot(dftmp, ggplot2::aes(x = X1, y = X2, color=selname)) + 
      ggplot2::geom_point( size = point.size, alpha = alpha) +
      ggplot2::scale_color_manual(values =  colsorig, name='Type') + themeVar +
      ggplot2::geom_text(ggplot2::aes(x = X1, y=X2, label=sel), data=clcnt, inherit.aes=FALSE, size=label.size, fontface = "bold") + guidesVar;
  } else {
    p <- ggplot2::ggplot(dftmp, ggplot2::aes(x = X1, y = X2, color=selname)) + 
      ggplot2::geom_point( size = point.size, alpha = alpha ) +
      ggplot2::scale_color_manual(values =  colsorig, name='Type') + themeVar + guidesVar;
  }

  invisible(p)
}

## Myeloid, match to myeloid from patient1
#' Read a pagoda2 selection file and return it as a factor
#' @description read a pagoda2 cell selection file and return it as a factor
#' while removing any mutliclassified cells
#' @param filepath name of the selection file
#' @param use.internal.name passed to factorFromP2Selection
#' @return a name factor with the membership of all the cells that are not multiclassified
#' @export readPagoda2SelectionAsFactor
readPagoda2SelectionAsFactor <- function(filepath,use.internal.name=FALSE) {
    factorFromP2Selection(removeSelectionOverlaps(
        readPagoda2SelectionFile(filepath)
    ),use.internal.name=use.internal.name)
}

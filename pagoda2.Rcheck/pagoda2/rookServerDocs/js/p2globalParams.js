"use strict";

/**
* @namespace
* This is an namespace that keeps all the startup and on going
* parameters for this pagoda app.
*/
var p2globalParams = {
  /*
    Specifies the data loading parameters for the application deployment
    This section controlls the behaviour of the global getDataLoadingParams
    function.
    
    THE VALUE HERE CAN BE OVERRIDEN BY URL ARGUMENT "dataconfiguration"
    configuration values:
      server: the application is running from a rook server and will connect getData.php and
        doComputation.php in the same directory to load the data and do computations respectively

      fileremote-static: The application will obtain data form a static url file
        that is hardcoded int he fileRemoteURL variable

      fileremote-from-url: The application will obtain data from a static file url
        and will not have access to an external data processing unit. The file will be specified
        in the URL as a parameter, so that a single app deployment with this option can access
        multiple files

      fileremote-user-select: The application will run from a remote file that the user can
        specify at application startup [NOT IMPLEMENTED]

      filelocal: The application will run for a local file using the the File API,
        the user will be presented with a prompt

      user-select: The user is presented with a prompt to select loading from any
        local or remote file [ NOT IMPLEMENTED ]
  */
  dataLoadingParams: {
    configuration: "server", // server | fileremote-static | fileremote-from-url | filelocal FUTURE:| fileremote-user-select | user-select
    fileRemoteURL: ""
  },

  misc: {
    jaxGeneQueryFormatString: '<a href="http://www.genecards.org/cgi-bin/carddisp.pl?gene={0}" target="_blank">{1}</a>'
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
    defaultPaletteName: 'pagodaOldAspect',
    defaultPaletteLevels: 100,
  },

  heatmapViewer: {
    fontBaseSize:  20,
    defaultPaletteLevels: 100,
    defaultPaletteName: 'pagodaOldHeatmap',
    defaultRowReordering: true,

    // From palette.js -- many more options
    // TODO: Extend to support parametrisable palettes and have different display
    // and internal names
    availablePalettes: {
      'pagodaOldAspect': {
              name: 'pagodaOldAspect',
              displayName: 'Pagoda Old Aspect',
              maxColors: 100,
              minColors: 100,
              generate: 'fixed-values',
              colorValues: [ "#006400","#0D6707","#166A0E","#1E6D15","#25701A","#2B7320","#317625","#36792A","#3C7C2F","#417F34","#468238","#4B853D","#4F8842","#548B47","#598E4B","#5E9150","#629455","#67975A","#6C9A5E","#709E63","#75A168","#7AA46D","#7EA772","#83AA76","#87AD7B","#8CB080","#91B385","#95B78A","#9ABA8F","#9EBD94","#A3C099","#A8C39E","#ACC6A3","#B1C9A8","#B6CDAD","#BAD0B2","#BFD3B7","#C3D6BC","#C8D9C2","#CDDDC7","#D1E0CC","#D6E3D1","#DBE6D7","#E0E9DC","#E4EDE1","#E9F0E6","#EEF3EC","#F3F6F1","#F7FAF6","#FCFDFC","#FFFDFC","#FFFBF7","#FFF9F3","#FFF6EE","#FFF4E9","#FFF2E5","#FFEFE0","#FFEDDB","#FFEBD7","#FFE8D2","#FFE6CE","#FFE3C9","#FFE1C4","#FFDFC0","#FFDDBB","#FFDAB7","#FFD8B2","#FFD6AE","#FFD3A9","#FFD1A5","#FFCFA0","#FFCC9B","#FFCA97","#FFC892","#FFC58E","#FFC389","#FFC185","#FFBE80","#FFBC7C","#FFBA77","#FFB773","#FFB56E","#FFB36A","#FFB165","#FFAE60","#FFAC5C","#FFAA57","#FFA752","#FFA54D","#FFA348","#FFA043","#FF9E3E","#FF9C39","#FF9933","#FF972E","#FF9527","#FF9220","#FF9018","#FF8E0E","#FF8C00" ]
      },
      'pagodaOldHeatmap': {
              name: 'pagodaOldHeatmap',
              displayName: 'Pagoda Old Heatmap',
              maxColors: 100,
              minColors: 100,
              generate: 'fixed-values',
              colorValues: ["#0000FF","#210CFF","#3116FF","#3D1EFF","#4725FF","#4F2BFF","#5730FF","#5E36FF","#653BFF","#6B40FF","#7145FF","#764AFF","#7B4FFF","#8154FF","#8559FF","#8A5DFF","#8F62FF","#9367FF","#976BFF","#9C70FF","#A075FF","#A479FF","#A87EFF","#AC83FF","#AF87FF","#B38CFF","#B790FF","#BA95FF","#BE9AFF","#C19EFF","#C5A3FF","#C8A7FF","#CBACFF","#CEB1FF","#D2B5FF","#D5BAFF","#D8BFFF","#DBC3FF","#DEC8FF","#E1CDFF","#E4D1FF","#E7D6FF","#EADBFF","#EDE0FF","#EFE4FF","#F2E9FF","#F5EEFF","#F8F3FF","#FAF7FF","#FDFCFF","#FFFDFC","#FFF9F7","#FFF5F1","#FFF1EC","#FFEDE7","#FFE9E2","#FFE6DC","#FFE2D7","#FFDED2","#FFDACD","#FFD6C8","#FFD2C3","#FFCEBE","#FFCAB9","#FFC6B4","#FFC3AF","#FFBFA9","#FFBBA5","#FFB7A0","#FFB39B","#FFAF96","#FFAB91","#FFA78C","#FFA387","#FF9F82","#FF9B7D","#FF9678","#FF9274","#FF8E6F","#FF8A6A","#FF8565","#FF8161","#FF7D5C","#FF7857","#FF7452","#FF6F4E","#FF6A49","#FF6544","#FF603F","#FF5B3A","#FF5636","#FF5031","#FF4B2C","#FF4427","#FF3E21","#FF361C","#FF2E16","#FF240F","#FF1707","#FF0000"]



      },

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
    },
  /* --- PARAMS END --- */
  
  /**
   * Update the params from the url arguments
   */  
  updateFromURL: function() {
    var urlParams = getWindowURLparams();
    if(typeof(urlParams.dataconfiguration) != 'undefined') {
       this.dataLoadingParams.configuration = urlParams.dataconfiguration;
    }
  }
  
};

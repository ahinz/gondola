var lon = -75.1651165;
var lat = 39.88;
var zoom = 10;
var map, layer;

var gd = {
    epsg4326: new OpenLayers.Projection("EPSG:4326"),
    epsg900913: new OpenLayers.Projection("EPSG:900913"),

    geturl: function(bounds) {
	bounds = bounds.transform(gd.map.getProjectionObject(),gd.epsg900913);
	var xmin = bounds.left;
	var xmax = bounds.right;
	var ymin = bounds.bottom;
	var ymax = bounds.top;

	path = "?xmin=" + xmin + "&ymin=" + ymin + "&xmax=" + xmax + "&ymax=" + ymax + "&width=256&height=256";

	var url = this.url;
	if (url instanceof Array) {
            url = this.selectUrl(path, url);
	}
	return url + path;	
    },

    init: function() {
	var options = {
	    projection: new OpenLayers.Projection("EPSG:900913"),
	    displayProjection: new OpenLayers.Projection("EPSG:4326"),
	    units: "m",
	    numZoomLevels: 22,
	    maxResolution: 156543.0339,
	    maxExtent: new OpenLayers.Bounds(-20037508, -20037508,
					     20037508, 20037508.34)
	};
	map = new OpenLayers.Map( 'map', options );
	layer = new OpenLayers.Layer.TMS( "Gondola",
					  "http://localhost:3000/image",
					  { 'type': 'png', 'getURL': gd.geturl, 'transparent': true });
	
	gd.map = map;

	layer.isBaseLayer = false;

	map.addControl(new OpenLayers.Control.LayerSwitcher());
    
	var gphy = new OpenLayers.Layer.Google(
            "Google Physical",
            {type: google.maps.MapTypeId.TERRAIN}
	);
	var gmap = new OpenLayers.Layer.Google(
            "Google Streets", // the default
            {numZoomLevels: 20}
	);
	var ghyb = new OpenLayers.Layer.Google(
            "Google Hybrid",
            {type: google.maps.MapTypeId.HYBRID, numZoomLevels: 20}
	);
	var gsat = new OpenLayers.Layer.Google(
            "Google Satellite",
            {type: google.maps.MapTypeId.SATELLITE, numZoomLevels: 22}
	);

	map.addLayers([gphy, gmap, ghyb, gsat, layer]);

	// Google.v3 uses EPSG:900913 as projection, so we have to
	// transform our coordinates
	map.setCenter(new OpenLayers.LonLat(lon, lat).transform(
            new OpenLayers.Projection("EPSG:4326"),
            map.getProjectionObject()
	), 5);
    }
}

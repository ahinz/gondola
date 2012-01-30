var lon = -75.1651165;
var lat = 39.88;
var zoom = 10;
var map, layer;

var gd = {
    epsg4326: new OpenLayers.Projection("EPSG:4326"),
    epsg900913: new OpenLayers.Projection("EPSG:900913"),

    altitude: 0,
    azimuth: 315,

    geturl: function(bounds) {
	bounds = bounds.transform(gd.map.getProjectionObject(),gd.epsg900913);
	var xmin = bounds.left;
	var xmax = bounds.right;
	var ymin = bounds.bottom;
	var ymax = bounds.top;
	
	path = "?xmin=" + xmin + "&ymin=" + ymin + "&xmax=" + xmax + "&ymax=" + ymax + "&width=256&height=256";
	path = path + "&altitude=" + gd.altitude + "&azimuth=" + gd.azimuth;
	
	var url = this.url;
	if (url instanceof Array) {
	    url = this.selectUrl(path, url);
	}
	
	return url + path;	
    },

    createLayer: function() {
	var layer = new OpenLayers.Layer.TMS( "Gondola",
					      "http://localhost:3000/image",
					      { 'type': 'png', 'getURL': gd.geturl, 'transparent': true });
	
	layer.isBaseLayer = false;
	return layer;
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

	var layer = new OpenLayers.Layer.TMS( "Gondola",
					  "http://localhost:3000/image",
					  { 'type': 'png', 'getURL': gd.geturl, 'transparent': true });
	
	gd.map = map;
	gd.layer = layer;

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
	map.setCenter(new OpenLayers.LonLat(-8376820.68453, 4877947.7539), 10);
    }
}

$(function() {
    $("#altitude").slider({
	min: 0,
	max: 7,
	value: 0,
	change: function(chg) {
	    var value = $("#altitude").slider("value");
	    $("#altitude_txt").val(value);
	    gd.altitude = value;
	    if (gd.layer) {
		gd.map.removeLayer(gd.layer);
		gd.layer = gd.createLayer();
		gd.map.addLayer(gd.layer);
	    }
	}
    });
    $("#azimuth").slider({
	min: 0,
	max: 360,
	value: 0,
	change: function(chg) {
	    var value = $("#azimuth").slider("value");
	    $("#azimuth_txt").val(value);
	    gd.azimuth = value;
	    if (gd.layer) {
		gd.map.removeLayer(gd.layer);
		gd.layer = gd.createLayer();
		gd.map.addLayer(gd.layer);
	    }
	}
    });

    $("#altitude").slider("value",0);
    $("#azimuth").slider("value",0);
});

// This section sets up the logic for event handling
var current_doc_clicked = { what: "nothing", element: undefined, object: undefined },
current_doc_hover = { what: "nothing", element: undefined, object: undefined },
old_doc_winning_state = { what: "nothing", element: undefined, object: undefined };

// global margins used for everything
var margin = {top: 30, right: 120, bottom: 30, left: 30},
clswidth = 700, 
topwidth = 200, 
width = clswidth + topwidth + margin.right + 2*margin.left,
height = 500,
clsheight = height,
topheight = clsheight;

// A few big global variables:
var clsData,  // (D rows, one for each document)
clsData2,  // (Document frequencies for each topic that shows up in any barchart)
topData;  // (Barchart widths for every topics #that shows up for the given lambda value)


function show_state()
{
    console.log(current_doc_clicked, current_doc_hover, old_doc_winning_state);
}

function reset_doc_state()
{
    current_doc_clicked = { what: "nothing", element: undefined, object: undefined },
    current_doc_hover = { what: "nothing", element: undefined, object: undefined },
    update_doc_drawing();    
}

function update_doc_drawing()
{
    var winning_state;
    if (current_doc_hover.what !== "nothing") {
        winning_state = current_doc_hover;
    } else {
        winning_state = current_doc_clicked;
    }

    if (old_doc_winning_state.element !== undefined) {
        switch (old_doc_winning_state.what) {
        case "nothing": throw new Error("internal error");
        case "cluster":
            cluster_doc_off.call(old_doc_winning_state.element, old_doc_winning_state.object);
            break;
        case "document":
            document_off.call(old_doc_winning_state.element, old_doc_winning_state.object);
            break;
        }
    
    }
    switch (winning_state.what) {
    case "nothing":
        document_off.call(old_doc_winning_state.element);
        break;
    case "cluster":
        cluster_doc_on.call(winning_state.element, winning_state.object);
        break;
    case "document":
        document_on.call(winning_state.element, winning_state.object);
        break;
    }
    old_doc_winning_state.what = winning_state.what;
    old_doc_winning_state.element = winning_state.element;
    old_doc_winning_state.object = winning_state.object;
    console.log(winning_state);
}


//////////////////////////////////////////////////////////////////////////////

// sort array according to a specified object key name 
// Note that default is decreasing sort, set decreasing = -1 for increasing
// adpated from http://stackoverflow.com/questions/16648076/sort-array-on-key-value
function fancysort(key_name, decreasing) {
    decreasing = (typeof decreasing === "undefined") ? 1 : decreasing;
    return function (a, b) {
	if (a[key_name] < b[key_name])
	    return 1*decreasing;
	if (a[key_name] > b[key_name])
	    return -1*decreasing;
	return 0;
    };
}

//////////////////////////////////////////////////////////////////////////////
// Bind onto data that is passed from shiny
var documentOutputBinding = new Shiny.OutputBinding();

$.extend(documentOutputBinding, {
    find: function(scope) {
	return $(scope).find('.shiny-document-output');
    },
    renderValue: function(el, data) {
	
	// adapted from https://github.com/timelyportfolio/shiny-d3-plot/blob/master/graph.js
	// idea is to turn an object of arrays into an array of objects
	// this is a bit more sophisticated since you don't have to specify the object key names

	// # of documnts
	var k = data['clsDat'].x.length
	
	clsData = [];
	for (var i=0; i < k; i++)  { 
            var obj = {};
            for (var key in data['clsDat']){
		obj[key] = data['clsDat'][key][i];
            }
            clsData.push( obj );
	}
	
	clsData2 = [];
	for (var i=0; i < data['clsDat2'].Topic.length; i++)  { 
            var obj = {};
            for (var key in data['clsDat2']){
		obj[key] = data['clsDat2'][key][i];
            }
            clsData2.push( obj );
	}
	
	topData = [];
	for (var i=0; i<data['topDat'].Topic.length; i++)  { 
            var obj = {};
            for (var key in data['topDat']){
		obj[key] = data['topDat'][key][i];
            }
            topData.push( obj );
	}
	
	// establish layout and vars for clsPlot
	var color = d3.scale.category10();
	
	// create linear scaling to pixels (and add some padding on outer region of scatterplot)
	var xrange = d3.extent(clsData, function(d){ return d.x; }); // d3.extent returns min and max of an array
	var xdiff = xrange[1] - xrange[0], xpad = 0.10;
	var xScale = d3.scale.linear()
            .range([0, clswidth])
            .domain([xrange[0] - xpad*xdiff, xrange[1] + xpad*xdiff]);
	
	var yrange = d3.extent(clsData, function(d){return d.y; });
	var ydiff = yrange[1] - yrange[0], ypad = 0.10;
	var yScale = d3.scale.linear()
            .range([clsheight, 0])
            .domain([yrange[0] - ypad*ydiff, yrange[1] + ypad*ydiff]);
	
	// remove the old graph
	var svg = d3.select(el).select("svg");      
	svg.remove();
	
	$(el).html("");
	
	// Create NEW svg element (that will contain everything)
	var svg = d3.select(el).append("svg")
			.attr("class", "cluster-svg") //needed to assign a class, so as to distinguish it from other svg for topic viz
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom);

			
	//tooltip for circles
	//var tip = d3.tip()
	  //.attr('class', 'd3-tip')
	  //.direction('nw')
      //.offset([-2*margin.top, -2*margin.left])
	  //.html(function(d) {
		//return "<strong>Document Name:</strong> <span style='color:red'>" + d.docnames + "</span>";
	  //})
	
	// This group is just for the cls plot
	var clssvg = svg.append("svg")
		.attr("width", clswidth + margin.left)
		.attr("height", clsheight + margin.top );

		
	//clssvg.call(tip);	
	
	var clsplot = clssvg.append("g")
	    .attr("id", "leftpanel")
            .attr("class", "points")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
	
	
	// moved this before the circles are drawn, so that hovering over a circle doesn't get interrupted
	// by hovering over one of the axis lines.      
	clsplot.append("line") // draw x-axis
            .attr("x1", 0)
            .attr("x2", clswidth)
            .attr("y1", clsheight/2) 
            .attr("y2", clsheight/2)
            .attr("stroke", "gray")
            .attr("opacity", 0.3);
	
	clsplot.append("line") // draw y-axis
            .attr("x1", clswidth/2) 
            .attr("x2", clswidth/2)
            .attr("y1", 0)
            .attr("y2", clsheight)
            .attr("stroke", "gray")
            .attr("opacity", 0.3);
	
	// Bind clsData to the points in the left panel:
	var points = clsplot.selectAll("points")
            .data(clsData)
            .enter();
	
	// text to indicate topic
	points.append("text")
            .attr("class", "txt")
            .attr("x", function(d) { return(xScale(+d.x)); })
            .attr("y", function(d) { return(yScale(+d.y) + 4); })
            .text(function(d) { return d.docs; })
            .attr("text-anchor", "middle")        
            .attr("stroke", "black")
            .attr("opacity", 1)
            .attr("font-size", 11)
            .attr("font-weight", 100);

	// Draw Voronoi map around cluster centers (if # of clusters > 1)
	// adapted from http://bl.ocks.org/mbostock/4237768  
	if (data['centers'].x.length > 1) {
            var centers = [];
            for (var i=0; i < data['centers'].x.length; i++)  { 
		centers[i] = [ xScale(data['centers'].x[i]), yScale(data['centers'].y[i]) ];
            }
            
            var voronoi = d3.geom.voronoi()
		.clipExtent([[0, 0], [clswidth, clsheight]])
		.x(function(d) { return(xScale(+d.x)); })
		.y(function(d) { return(yScale(+d.y)); });
                // .clipExtent([[margin.left, margin.top], [clswidth - margin.right, clsheight - margin.bottom]]);
	    
            var vdat = voronoi(clsData);
	    
            var cluster_paths = [];
            for (i=0; i < data['centers'].x.length; ++i) {
		cluster_paths[i] = "";
            }
            for (i=0; i < clsData.length; ++i) {
		var cluster = Number(clsData[i].cluster) - 1;
		cluster_paths[cluster] = cluster_paths[cluster] + "M" + vdat[i].join("L") + "Z";
            }

	    // Append the Voronoi group to the clsplot group:
            clsplot.append("g")
		.attr("id", "voronoi-group");
            
            clsplot.select("#voronoi-group")
		.selectAll("path")
		.data(cluster_paths)
		.enter()
		.append("path")
		.style("fill", function(d, i) { return d3.rgb(color(String(i+1))).brighter(1.5); })
                // .style("stroke", function(d, i) { return d3.rgb(color(String(i+1))).brighter(1.5); })
		.style("fill-opacity", 0.3)
		.attr("d", function(d) { return d; })
		.on("mouseover", function(d, i) {
                    current_doc_hover.element = this;
                    current_doc_hover.what = "cluster";
                    current_doc_hover.object = String(i+1);
                    update_doc_drawing();
		})
		.on("mouseout", function(d, i) {
                    current_doc_hover.element = undefined;
                    current_doc_hover.what = "nothing";
                    current_doc_hover.object = undefined;
                    update_doc_drawing();
		})
		.on("click", function(d, i) {
                    current_doc_clicked.element = this;
                    current_doc_clicked.what = "cluster";
                    current_doc_clicked.object = String(i+1);
                    update_doc_drawing();
		});
	}

	// draw the circles:
	points.append("circle")
            .attr("class", "cluster-doc")
            // Setting the id will allow us to select points via the value of selectInput
            .attr("id", function(d) { return "Document"+d.docs; })
            .style("opacity", 0.3)
            .style("fill", function(d) { return color(d.cluster); })
            // circle sizes should get smaller as the # of topics increases
            .attr("r", function(d) { return (10000/k)*Math.sqrt(d.Freq) ; })  
            .attr("cx", function(d) { return (xScale(+d.x)); })
            .attr("cy", function(d) { return (yScale(+d.y)); })
            .on("mouseover", function(d) {
					//tip.show(d);
					current_doc_hover.element = this;
					current_doc_hover.what = "document";
					current_doc_hover.object = d;
					update_doc_drawing();
            })  // highlight circle and print the relevant proportion within circle
            .on("click", function(d) {
					current_doc_clicked.element = this;
					current_doc_clicked.what = "document";
					current_doc_clicked.object = d;
					update_doc_drawing();
            })
            .on("mouseout", function(d) {
					//tip.hide(d);
					current_doc_hover.element = undefined;
					current_doc_hover.what = "nothing";
					current_doc_hover.object = d;
					update_doc_drawing();
            });
	
	// moved this below the drawing of the circles so that if a circle occludes the 'clear selection' link, 
	// the user can still click on the link to clear the selection.
	svg.append("text")
            .text("Click here to clear selection")
            .attr("x", clswidth - 350)
            .attr("y", 10)
            //.attr("font-weight", "bold")
			.attr("font-weight", 100)
            .attr("cursor", "pointer")
            .on("click", function() {
                reset_doc_state();
            });
	
	// Basic text to tell user to click on stuff!!!
	svg.append("text")
            .text("Click elements below to freeze selection")
            .attr("x", 10)
            .attr("y", 10)
	
	// establish layout and vars for bar chart	
	var barDefault2 = topData.filter(function(d) { return d.Category == "Default" });
	
	var y = d3.scale.ordinal()
            .domain(barDefault2.map(function(d) { return d.Topic; }))
            .rangeRoundBands([0, topheight], 0.15);
	var x = d3.scale.linear()
            .domain([1, d3.max(barDefault2, function(d) { return d.Total; })])
            .range([0, topwidth])
            .nice();
	var color2 = d3.scale.category10();
	var yAxis  = d3.svg.axis()
            .scale(y);
	
	// Add a group for the barchart:
	var chart = svg.append("g")
	    .attr("id", "cluster-bar-freqs") // place bar chart to the right of the cls plot;
        .attr("transform", "translate(" + +(clswidth + 2*margin.left + 50) + "," + 2*margin.top + ")"); 
	// + 50 above to provide a bit of width between the two panels to display the topics

	// Bind 'default' data to 'default' bar chart
	var basebars = chart.selectAll(".cluster-bar-totals")
            .data(barDefault2)
            .enter();
	
	// Draw the gray background bars defining the overall frequency of each word
	basebars    
            .append("rect")
            .attr("class", "cluster-bar-totals")
            .attr("x", 0)  
            .attr("y", function(d) { return y(d.Topic); })
            .attr("height", y.rangeBand()) 
            .attr("width", function(d) { return x(d.Total); } )
            .style("fill", "gray")  
            .attr("opacity", 0.4);  
	
	//Add topic labels
	basebars
            .append("text")
            .attr("x", -5)
            .attr("class", "topics")
            .attr("y", function(d) { return y(d.Topic) + 10; })
            .attr("text-anchor", "end") // right align text - use 'middle' for center alignment
            .attr("dominant-baseline", "middle") //vertical alignment
            .text(function(d) { return d.Topic; })
            .on("mouseover", text_doc_on)
            .on("mouseout", text_doc_off);

	        // add a 'title' to bar chart 
	svg.append("text")
            .attr("x", clswidth + 2*margin.left + topwidth/2)
            .attr("y", -margin.top/2)
            .attr("text-anchor", "middle")
            .attr("class", "cluster-bubble-tool")       //set class so we can remove it when highlight_off is called  
            .style("font-size", "16px") 
            .style("text-decoration", "underline")  
            .text("Most salient topics");		
			
	// adapted from http://bl.ocks.org/mbostock/1166403
	var xAxis = d3.svg.axis().scale(x)
            .orient("top")
            .tickSize(-topheight)
            .tickSubdivide(true)
	    .ticks(6);
	
	chart.attr("class", "xaxis")
	    .call(xAxis);

	/*
	// update drawing based on a selected topic in selectInput
	if (data['currentTopic'] != 0) { //0 represents no topic selected
	var currentTopic = d3.select("#Topic"+data['currentTopic'])[0][0];
	console.log(currentTopic);
	current_doc_clicked.element = currentTopic;
	current_doc_clicked.what = "topic";
	current_doc_clicked.object = currentTopic.__data__;
	update_doc_drawing();
	} else
	// Have to update drawing in the case where shiny inputs have changed
	// but no mouse hover/clicks have happened (on the plot itself)
	*/
	
	//Create zooming functionality
	var zoom = d3.behavior.zoom()
	.x(xScale)
    .y(yScale)
    .scaleExtent([0.5, 10])
    .on("zoom", function() {
		var scale = d3.event.scale,
			translation = d3.event.translate,
			tbound = -clsheight * scale,
			bbound = clsheight * scale,
			//lbound = (-clswidth + margin.right ) * scale,
			lbound = (-clswidth - margin.right ) * scale,
			//rbound = (clswidth - margin.left) * scale,
			rbound = (clswidth + margin.left) * scale,
			//tx = Math.max(Math.min(translation[0], rbound), lbound),
			ty = Math.max(Math.min(translation[1], bbound), tbound);
			tx = translation[0],
			
			translation = [tx, ty];
		
			zoom.translate(translation);
			
			clsplot
			.attr("transform", "translate(" + translation + ")" +
				  " scale(" + scale + ")");
			
			//var k = clsData.length 
					
			clsplot.selectAll("circle")
				//.data(clsData)
				.transition()
				.attr("r", function(d){ return (10000/k)*Math.sqrt(d.Freq)/scale; });
				
			clsplot.selectAll("text")
				.transition()
				.style("font-size", 11/scale)
				.style("stroke-width", 1 / scale);
	});	

	clssvg.call(zoom);

	update_doc_drawing();
	
    }
    
});

Shiny.outputBindings.register(documentOutputBinding, 'cpsievert.scatterbinding');

function cluster_doc_on(d) {
    
    // increase opacity of Voronoi region
    var circle = d3.select(this);
    circle.style("fill-opacity", 0.5)
    var cluster = d;
    
    // remove the title 
    var text = d3.select(".cluster-bubble-tool");
    text.remove();
    
    // clustDat contains the topics in this cluster -- just need this to compute % of tokens from
    // the cluster, to display above barchart:
    var clustDat = clsData.filter(function(d) { return d.cluster == cluster });
    
    var Freq = 0;
    for (var i=0; i < clustDat.length; i++) {
	Freq = Freq + clustDat[i]['Freq'];
    }
    var Freq = Freq.toFixed(3); // round to three decimal place
    
    // append a 'title' to bar chart with data relevant to the cluster of interest
    d3.select(".cluster-svg")
    	.append("text")
    	.attr("x", clswidth + 2*margin.left + topwidth/2)             
    	.attr("y", margin.top/2)
    	.attr("text-anchor", "middle")
    	.attr("class", "cluster-bubble-tool")     
    	.style("font-size", "16px") 
    	.style("text-decoration", "underline")  
    	.text(Freq + "% of the corpus comes from cluster " + cluster);

    // filter the bars according to the selected cluster
    var dat2 = topData.filter(function(d) { return d.Category == "Cluster"+cluster });

    var y = d3.scale.ordinal()
        .domain(dat2.map(function(d) { return d.Topic; }))
        .rangeRoundBands([0, topheight], 0.15);
    var x = d3.scale.linear()
        .domain([1, d3.max(dat2, function(d) { return d.Total; })])
        .range([0, topwidth])
        .nice();

    // Change Total Frequency bars
    d3.selectAll(".cluster-bar-totals")
	.data(dat2)
	.transition()
	.attr("x", 0)  
	.attr("y", function(d) { return y(d.Topic); })
	.attr("height", y.rangeBand()) 
	.attr("width", function(d) { return x(d.Total); } )
	.style("fill", "gray")   
	.attr("opacity", 0.4);

    // Change topic labels
    d3.selectAll(".topics")
	.data(dat2)
	.transition()
	.text(function(d) { return d.Topic; });

    // Create blue bars (drawn over the gray ones) to signify the frequency under the selected cluster
    d3.select("#cluster-bar-freqs")
	.selectAll(".overlay")  
	.data(dat2)
	.enter()
	.append("rect")
	.attr("class", "overlay")
	.attr("x", 0)  
	.attr("y", function(d) { return y(d.Topic); })
	.attr("height", y.rangeBand()) 
	.attr("width", function(d) { return x(d.Freq); } )
	.style("fill", "steelblue")   
	.attr("opacity", 0.4); 

    // adapted from http://bl.ocks.org/mbostock/1166403
    var xAxis = d3.svg.axis().scale(x)
        .orient("top")
        .tickSize(-topheight)
        .tickSubdivide(true)
	.ticks(6);

    // redraw x-axis
    d3.selectAll(".xaxis")
	.attr("class", "xaxis")
	.call(xAxis);

}


function document_on(d) {

    // Increase opacity of currently selected circle
    //var circle = d3.select("#Document"+d.documents);
    var circle = d3.select(this);
    circle.style("opacity", 0.8);
	//TODO Add a tooltip to display information about document
	circle.append("svg:title")
			.text(function(d) { return d.Freq; });

	
    var Freq = d.Freq.toFixed(3), documents = d.docs, cluster = d.cluster, docnames = d.docnames;

    // remove the title with cluster proportion
    var text = d3.select(".cluster-bubble-tool");
    text.remove();

    // append text with info relevant to topic of interest
    d3.select(".cluster-svg")
    	.append("text")
    	.attr("x", clswidth + 2*margin.left + topwidth/2)             
    	.attr("y", margin.top/2)
    	.attr("text-anchor", "middle")
    	.attr("class", "cluster-bubble-tool")       // set class so we can remove it when highlight_off is called  
    	.style("font-size", "16px") 
    	.style("text-decoration", "underline")  
    	.text(Freq + "% of the corpus comes from document " + docnames);

    // grab the bar-chart data for this topic only:
    var dat2 = topData.filter(function(d) { return d.Category == "Document"+documents });

    var y = d3.scale.ordinal()
        .domain(dat2.map(function(d) { return d.Topic; }))
        .rangeRoundBands([0, topheight], 0.15);
    var x = d3.scale.linear()
        .domain([1, d3.max(dat2, function(d) { return d.Total; })])
        .range([0, topwidth])
        .nice();

    // remove the blue bars of cluster frequencies
    d3.selectAll(".overlay").remove();

    // Change Total Frequency bars
    d3.selectAll(".cluster-bar-totals")
	.data(dat2)
	.transition()
        .attr("x", 0)  
        .attr("y", function(d) { return y(d.Topic); })
        .attr("height", y.rangeBand()) 
        .attr("width", function(d) { return x(d.Total); } )
        .style("fill", "gray")   
        .attr("opacity", 0.4);

    // Change topic labels
    d3.selectAll(".topics")
	.data(dat2)
	.transition()
        .text(function(d) { return d.Topic; });

    // Create red bars (drawn over the gray ones) to signify the frequency under the selected topic
    d3.select("#cluster-bar-freqs").selectAll("overlay")  
	.data(dat2)
	.enter()
        .append("rect")
        .attr("class", "overlay")
        .attr("x", 0)  
        .attr("y", function(d) { return y(d.Topic); })
        .attr("height", y.rangeBand()) 
        .attr("width", function(d) { return x(d.Freq); } )
        .style("fill", "red")   
        .attr("opacity", 0.4); 

    // adapted from http://bl.ocks.org/mbostock/1166403
    var xAxis = d3.svg.axis().scale(x)
        .orient("top")
        .tickSize(-topheight)
        .tickSubdivide(true)
	.ticks(6);

    d3.selectAll(".xaxis")
	.attr("class", "xaxis")
	.call(xAxis);

}

function cluster_doc_off(d) {

    var circle = d3.select(this);
    circle.style("fill-opacity", 0.4);  // go back to original opacity

    // change to default title
    d3.selectAll(".cluster-bubble-tool").text("Most salient topics");

    // remove the blue bars of cluster frequencies
    d3.selectAll(".overlay").remove();

    // go back to 'default' bar chart
    var dat2 = topData.filter(function(d) { return d.Category == "Default" });

    var y = d3.scale.ordinal()
        .domain(dat2.map(function(d) { return d.Topic; }))
        .rangeRoundBands([0, topheight], 0.15);
    var x = d3.scale.linear()
        .domain([1, d3.max(dat2, function(d) { return d.Total; })])
        .range([0, topwidth])
        .nice();

    // Change Total Frequency bars
    d3.selectAll(".cluster-bar-totals")
	.data(dat2)
	.transition()
        .attr("x", 0)  
        .attr("y", function(d) { return y(d.Topic); })
        .attr("height", y.rangeBand()) 
        .attr("width", function(d) { return x(d.Total); } )
        .style("fill", "gray")   
        .attr("opacity", 0.4);

    // Change word labels
    d3.selectAll(".topics")
	.data(dat2)
	.transition()
        .text(function(d) { return d.Topic; });

    // adapted from http://bl.ocks.org/mbostock/1166403
    var xAxis = d3.svg.axis()
	.scale(x)
        .orient("top")
        .tickSize(-topheight)
        .tickSubdivide(true)
	.ticks(6);

    // redraw x-axis
    d3.selectAll(".xaxis")
	.attr("class", "xaxis")
	.call(xAxis);
    
}

function document_off(d) {
    //var circle = d3.select("#Topic"+d.topics);
    var circle = d3.select(this);
    circle.style("opacity", 0.4);  // go back to original opacity

    // change to default title
    d3.selectAll(".cluster-bubble-tool").text("Most salient topics");
    
    // remove the blue bars of cluster frequencies
    d3.selectAll(".overlay").remove();

    // go back to 'default' bar chart
    var dat2 = topData.filter(function(d) { return d.Category == "Default" });

    var y = d3.scale.ordinal()
        .domain(dat2.map(function(d) { return d.Topic; }))
        .rangeRoundBands([0, topheight], 0.15);
    var x = d3.scale.linear()
        //.domain([1, d3.max(dat2, function(d) { return d.Total + 100; })])
		.domain([1, d3.max(dat2, function(d) { return 100; })])
        .range([0, topwidth])
        .nice();

    // Change Total Frequency bars
    d3.selectAll(".cluster-bar-totals")
	.data(dat2)
	.transition()
        .attr("x", 0)  
        .attr("y", function(d) { return y(d.Topic); })
        .attr("height", y.rangeBand()) 
        .attr("width", function(d) { return x(d.Total); } )
        .style("fill", "gray")   
        .attr("opacity", 0.4);

    // Change topic labels
    d3.selectAll(".topics")
	.data(dat2)
	.transition()
        .text(function(d) { return d.Topic; });

    // adapted from http://bl.ocks.org/mbostock/1166403
    var xAxis = d3.svg.axis().scale(x)
        .orient("top")
        .tickSize(-topheight)
        .tickSubdivide(true)
	.ticks(6);
    
    // redraw x-axis
    d3.selectAll(".xaxis")
	.attr("class", "xaxis")
	.call(xAxis);    
}

function text_doc_on(d) {
    var text = d3.select(this);
    text.style("font-weight", "bold");

    var Topic = d.Topic;
    var dat2 = clsData2.filter(function(d) { return d.Topic == Topic });

    // # of documents
    var k = dat2.length  

    //Change size of bubbles according to the topics's distribution over documents
    d3.selectAll(".cluster-doc")
	.data(dat2)
	.transition()
        .attr("r", function(d) { return (5000/k)*Math.sqrt(d.Freq); });
}

function text_doc_off() {
    var text = d3.select(this);
    text.style("font-weight", null);

    var k = clsData.length  // # of documents

    d3.selectAll(".cluster-doc")
	.data(clsData)
	.transition()
		//.attr("r", function(d) { return (k)*Math.sqrt(d.Freq); });
        .attr("r", function(d) { return (10000/k)*Math.sqrt(d.Freq); });
	   //.attr("r", function(d) { return (1000/k)*Math.sqrt(50); });
}


var lineChart = function(loc,data,config){
  var c = this
  this.yVal = config.y
  this.xVal = config.x
  this.quartile = config.quartile
  this.groupedData = {}
    data.forEach(function(d){ 
       c.groupedData[d.runNumber] != undefined? c.groupedData[d.runNumber].push(d): c.groupedData[d.runNumber] = [d]
       })
  this.data = data
  this.loc = loc
  this.legend = config.legend
  this.margin = {top: 10, right: 100, bottom: 60, left: 80};
  this.width= 650;
  this.height = 400;
  this.legendEntryHeight  = 30;
  this.xTitle = config.xTitle
  this.yTitle = config.yTitle
  this.x= d3.scaleLinear().range([0, this.width]).domain([d3.min(c.data, function(d) { return d[c.xVal]; }), d3.max(c.data, function(d) { return d[c.xVal]; })])
  this.y = d3.scaleLinear().domain([d3.min(c.data,function(d){ return c.yVal.map(x=> d[x]).reduce(function(a,b){return Math.min(a,b)});}), d3.max(c.data,function(d){ return c.yVal.map(x=> d[x]).reduce(function(a,b){return Math.max(a,b)});})]).rangeRound([c.height, 0]);
  this.axis = d3.axisBottom(this.x).ticks(20);
  this.yaxis = d3.axisLeft(this.y);


}

lineChart.prototype.draw = function(){

    var c = this
    var legend = this.loc.append("div").attr("class","legend")
    var g = legend.append("svg").attr("height",c.legendEntryHeight * c.yVal.length ).attr("width","100%").selectAll(".leg-entry").data(c.yVal)
        .enter()
        .append("g")


        g.append("rect")
          .attr("class",function(d, i){ return d + " leg-entry leg c" + i})
          .attr("height", c.legendEntryHeight)
          .attr("width", c.legendEntryHeight)
          .attr("y", function(d,i){ return c.legendEntryHeight * i})
        g.append("text")
         .text(function(d){ return d})
         .attr("alignment-baseline","middle")
         .style("white-space", "pre")
         .attr("y", function(d,i){ return (c.legendEntryHeight * i) + c.legendEntryHeight / 2})
         .attr("x", function(d,i){ return c.legendEntryHeight + 10})

    var div = this.loc.append("div").attr("class", "chart")
    var g = div.select("g");
    if(g.empty()){
      // Create the skeletal chart.
        svg = div.append("svg")
            .attr("width", "100%")
            .attr("height", c.height + c.margin.top + c.margin.bottom)
          g = svg.append("g")
            .attr("transform", "translate(" + c.margin.left + "," + c.margin.top + ")");

      g.append("g")
          .attr("class", "axis axis--x")
          .attr("transform", "translate(0," + c.height + ")")
          .call(d3.axisBottom(c.x));

      g.append("g")
          .attr("class", "axis axis--y")
          .call(d3.axisLeft(c.y))
      
      svg.append("text")
          .attr("class", "axis-title")
          .attr("transform", "translate(0," + c.height / 2 + ")rotate(-90)")
          .attr("dy", ".71em")
          .style("text-anchor", "middle")
          .text(c.yTitle);

      svg.append("text")
          .attr("class", "axis-title")
          .attr("transform", "translate(" + c.width / 2 + ", " + (c.height + c.margin.bottom) + ")")
          .attr("dy", ".71em")
          .style("text-anchor", "middle")
          .text(c.xTitle);


      g.append("g")
        .attr("class","focus-group")

      if(c.quartile){
      var area = d3.area()
              .curve(d3.curveMonotoneX)
              .x(function(d) { return c.x(d[c.xVal]); })
              .y0(function(d) { return c.y(d["lowerQuartile"]); })
              .y1(function(d) { return c.y(d["upperQuartile"]); });

      g.append("g").append("path")
          .datum(c.data)
          .attr("class", "area")
          .attr("d", area);
      }
      c.yVal.forEach(function(m,mi){

        Object.keys(c.groupedData).forEach(function (dk){
         
          var line = d3.line()
              .curve(d3.curveMonotoneX)
              .x(function(d) {
                  return c.x(d[c.xVal]);
                })
              .y(function(d) {
                  return c.y(d[m]);
                });
          var series = g.append("g")
          series.append("path")
              .datum(c.groupedData[dk])
              .attr("class", function(d,i){
                return "line series c" + mi
                })
              .attr("d", line);

          series.append("text")
              .attr("transform", function(d) { return "translate(" + c.x(c.data[c.data.length-1][c.xVal]) + "," + c.y(c.data[c.data.length-1][m]) + ")"; })
              .attr("x", 3)
              .attr("dy", "0.35em")
              .style("font", "10px sans-serif")
              .text(function(d) { return m; });

        })
      })


      var t = d3.merge(c.yVal.map(function(yv){
        return c.data.map(function(d){
          return {"x": d[c.xVal], "y": d[yv], "all":d}
        })}))
      var voronoi = d3.voronoi()
        .x(function(d) { return c.x(d.x); })
        .y(function(d) { return c.y(d.y); })
        .extent([[-1, -1], [c.width + 1, c.height + 1]]);

      var voronoiGroup = g.append("g")
        .attr("class", "voronoi");

      voronoiGroup.selectAll("path")
        .data(voronoi.polygons(t))
        .enter().append("path")
        .attr("d", function(d) { return d ? "M" + d.join("L") + "Z" : null; })
        .on("mouseover", function(d){
            var pointData = d.data.all
            var pointX = d.data.x

          legend.selectAll("text").data(c.yVal).text(function(e){
            return e + "  -  " + pointData[e]
            })

            var label = g.select(".focus-group").selectAll(".focus").data(c.yVal).enter().append("g").attr("class","focus")
                .attr("transform", function(d){
              return "translate(" + c.x(pointX) + "," + c.y(pointData[d]) + ")";
            })
            label.append("circle")
            .transition().duration(250).style("fill-opacity", 1)
            .attr("r",3.5)
            label.append("text")
                .attr("x",5)
                .attr("y",-5)
                .text(function(d){
                  return pointData[d]
                })
                .transition().duration(250).style("fill-opacity", 1);

        })
        .on("mouseout",function(d){
            g.selectAll(".focus").remove()
            var pointData = d.data.all
            var pointX = d.data.x

          legend.selectAll("text").data(c.yVal).text(function(e){
            return e
            })
        })

    }
  }

var lc = new lineChart(d3.select("#statsViewer"), d, {"quartile": false, "y":["bestsse"], "x": "generationNumber","xTitle":"Generation", "yTitle":"Best Fitness Value" }).draw()
//d
// {{"quartile": false, "y":["lowerQuartile"], "x": "month","xtitle":"Months", ytitle:"price" }

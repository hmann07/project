

var svg = d3.select("svg"),
    width = +svg.attr("width"),
    height = +svg.attr("height");

svg.append('svg:defs').append('svg:marker')
    .attr('id', 'end-arrow')
   .attr("viewBox", "0 -5 10 10")
    .attr("refX", 20)
    .attr("refY", 0)
    .attr("markerWidth", 4)
    .attr("markerHeight", 4)
    .attr("orient", "auto")
  .append('svg:path')
    .attr("d", "M0,-5L10,0L0,5")
    .attr('fill', '#666666');





var tnodes = [{id: "5", reflexive: false, layer: 0.5}, {id: "1", reflexive: false, layer: 0.0}, {id: "6", reflexive: false, layer: 0.25}, {id: "13", reflexive: false, layer: 0.125}, {id: "2", reflexive: false, layer: 0.0}, {id: "3", reflexive: false, layer: 1.0}, {id: "23", reflexive: false, layer: 0.1875}, {id: "8", reflexive: false, layer: 0.25}, {id: "4", reflexive: false, layer: 0.5}]
var links = [{source: "3", target: "6" , left: false, right: true, weight: 1.1934151118692853 }, {source: "4", target: "4" , left: false, right: true, weight: 1.1934151118692853 } , {source: "5", target: "3" , left: false, right: true, weight: 2.2602454611148635 } , {source: "8", target: "8" , left: false, right: true, weight: -4.205703852364375 } , {source: "6", target: "5" , left: false, right: true, weight: -1.1956821800453372 } , {source: "1", target: "13" , left: false, right: true, weight: -0.05419891682539668 } , {source: "8", target: "4" , left: false, right: true, weight: 1.1806183823958682 } , {source: "13", target: "23" , left: false, right: true, weight: 0.24118084951948493 } , {source: "13", target: "8" , left: false, right: true, weight: -0.05643280451975091 } , {source: "2", target: "6" , left: false, right: true, weight: -1.0347599254770872 } , {source: "23", target: "8" , left: false, right: true, weight: 0.08088318251915272 } , {source: "4", target: "3" , left: false, right: true, weight: -3.0254031052170665 } , {source: "1", target: "5" , left: false, right: true, weight: 2.0155587830911963 } ]
var nodeById = d3.map(tnodes, function(d) { return d.id; })
var bilinks = [];

links.forEach(function(link) {
    var s = link.source = nodeById.get(link.source),
        t = link.target = nodeById.get(link.target),
        i = {layer: (s.layer + t.layer) / 6 },
        i2 = {layer: (s.layer + t.layer) / 3 }; // intermediate node
    tnodes.push(i)
    tnodes.push(i2);
    links.push({source: s, target: i}, {source: i, target: i2} , {source: i2, target: t});
    bilinks.push([s, i, i2 ,t]);
  });



var layerScale = d3.scaleLinear().range([40 ,width - 40]).domain([0,1])

var nodes = tnodes.map(function(d){
	d.fx = layerScale(d.layer)
	return d
})

var simulation = d3.forceSimulation()
	.force("link", d3.forceLink())
    .force("charge", d3.forceManyBody().strength(-30))
    .force("collision", d3.forceCollide(1))
    .force("center", d3.forceCenter(width / 2, height / 2));

simulation
      .nodes(nodes)
      .on("tick", ticked);

  simulation.force("link")
      .links(links);

  var link = svg.selectAll(".link")
    .data(bilinks)
    .enter().append("path")
      .attr("class", "link")
      //.style('marker-start', function(d) { return d.left ? 'url(#start-arrow)' : ''; })
      .style('marker-end', 'url(#end-arrow)');

  var node = svg.selectAll(".node")
    .data(nodes.filter(function(d) { return d.id; }))
    .enter().append("circle")
      .attr("class", "node")
      .attr("r", 15)
      .attr("fill", "#1f77b4")
      //.attr("fill", function(d) { return color(d.group); })
      .call(d3.drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended));

  node.append("title")
      .text(function(d) { return d.id + " - " + d.layer; });

  function ticked() {
    link.attr("d", positionLink);
    node.attr("transform", positionNode);
  }


function positionLink(d) {
  return "M" + d[0].x + "," + d[0].y
       + "S" + d[1].x + "," + d[1].y
       + " " + d[2].x + "," + d[2].y;
}

function positionNode(d) {
  return "translate(" + d.x + "," + d.y + ")";
}

function dragstarted(d) {
  if (!d3.event.active) simulation.alphaTarget(0.3).restart();
  d.fx = d.x, d.fy = d.y;
}

function dragged(d) {
  d.fx = d3.event.x, d.fy = d3.event.y;
}

function dragended(d) {
  if (!d3.event.active) simulation.alphaTarget(0);
  d.fx = null, d.fy = null;
}

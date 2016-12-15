var margin = {
  top: 20,
  bottom: 20,
  left: 20,
  right: 20
}
var width = 500;
var height = 500;

var svg = d3.select('#vis')
              .append('svg')
              .attr('width', width - margin.left - margin.right)
              .attr('height', height - margin.top - margin.bottom)
              .append('g')
              .attr('transform', 'translate(' + margin.left + ', ' + margin.top + ')')

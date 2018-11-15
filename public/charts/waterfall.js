// From http://bl.ocks.org/chucklam/f3c7b3e3709a0afd5d57
// Or as a Gist https://gist.github.com/chucklam/f3c7b3e3709a0afd5d57

function transformData (data) {
  // Transform data (i.e., finding cumulative values and total) for easier charting
  return data.reduce(function (accu, item) {
    var newItem = Object.assign({}, item)
    if (newItem.isSubtotal) {
      newItem.start = 0
      newItem.end = accu.cumulative
      newItem.class = 'total'
    } else {
      newItem.start = accu.cumulative
      accu.cumulative += newItem.value
      newItem.end = accu.cumulative
      newItem.class = (newItem.value >= 0) ? 'positive' : 'negative'
    }
    accu.items.push(newItem)
    return accu
  }, {cumulative: 0, items: []}).items
}

function getProperty (propertyName) {
  return function getPropertyInner (d) {
    return d[propertyName]
  }
}

// eslint-disable-next-line no-unused-vars
function waterfallChart (options) {
  var containerSelector = options.containerSelector
  var margin = options.margin || {top: 20, right: 0, bottom: 30, left: 60}
  var viewPort = options.viewPort || {width: 960, height: 500}
  var height = viewPort.height - margin.top - margin.bottom
  var width = viewPort.width - margin.left - margin.right

  var barPadding = 0.3

  var formatNumber = d3.format('.0s')

  function barText (d) {
    var delta = d.end - d.start
    var sign = d.class === 'positive'
      ? '+'
      : '' // class === 'total' or 'negative' (which already renders '-')
    return delta === 0
      ? ''
      : sign + ' ' + formatNumber(delta)
  }

  var chart = d3.select(containerSelector).append('svg')
    .attr('width', viewPort.width)
    .attr('height', viewPort.height)
    .append('g')
    .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')')

  function update (options) {
    var data = transformData(options.data)
    if (options.viewPort) {
      height = options.viewPort.height - margin.top - margin.bottom
      width = options.viewPort.width - margin.left - margin.right
    }

    var x = d3.scaleBand()
      .domain(data.map(getProperty('name')))
      .rangeRound([0, width])
      .padding(barPadding)
    var y = d3.scaleLinear()
      .domain([0, d3.max(data, getProperty('end'))])
      .range([height, 0])

    chart.select('.x.axis').remove()
    chart.append('g')
      .attr('class', 'x axis')
      .attr('transform', 'translate(0,' + height + ')')
      .call(d3.axisBottom(x))
    chart.select('.y.axis').remove()
    chart.append('g')
      .attr('class', 'y axis')
      .call(d3.axisLeft(y).tickFormat(formatNumber))

    var bar = chart.selectAll('.bar')
      .data(data, getProperty('name'))

    var gEnter = bar.enter().append('g')
      .attr('class', function (d) { return 'bar ' + d.class })
      .attr('transform', function (d) { return 'translate(' + x(d.name) + ',0)' })
    gEnter.append('rect')
      .attr('width', x.bandwidth())
      .attr('y', function (d) { return y(d.start) })
      .transition()
      .attr('height', function (d) { return Math.abs(y(d.start) - y(d.end)) })
      .attr('y', function (d) { return y(Math.max(d.start, d.end)) })
    gEnter.append('text')
      .text(barText)
      .attr('x', function ( /* d */) { return x.bandwidth() / 2 })
      .transition()
      .attr('dy', function (d) { return (d.class === 'negative' ? '-' : '') + '.75em' })
      .attr('y', function (d) { return y(d.end) + 5 })
    gEnter.filter(function (d) { return d.class != 'total' }).append('line')
      .attr('class', 'connector')
      .attr('x1', x.bandwidth() + 5)
      .attr('y1', function (d) { return y(d.end) })
      .attr('x2', x.bandwidth() / (1 - barPadding) - 5)
      .attr('y2', function (d) { return y(d.end) })

    bar
      .attr('class', function (d) { return 'bar ' + d.class })
      .transition()
      .attr('transform', function (d) { return 'translate(' + x(d.name) + ',0)' })
    bar.select('rect')
      .transition()
      .attr('height', function (d) { return Math.abs(y(d.start) - y(d.end)) })
      .attr('width', x.bandwidth())
      .attr('y', function (d) { return y(Math.max(d.start, d.end)) })
    bar.select('text')
      .text(barText)
      .transition()
      .attr('dy', function (d) { return (d.class === 'negative' ? '-' : '') + '.75em' })
      .attr('x', function ( /* d */) { return x.bandwidth() / 2 })
      .attr('y', function (d) { return y(d.end) + 5 })
    bar.select('.connector')
      .transition()
      .attr('x1', x.bandwidth() + 5)
      .attr('y1', function (d) { return y(d.end) })
      .attr('x2', x.bandwidth() / (1 - barPadding) - 5)
      .attr('y2', function (d) { return y(d.end) })

    bar.exit().remove()
  }

  update({data: options.data})

  return update
}

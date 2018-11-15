var requestAnimationFrame = typeof window.requestAnimationFrame !== 'undefined'
  ? window.requestAnimationFrame
  : function (callback) { setTimeout(function () { callback() }, 0) }

// eslint-disable-next-line no-unused-vars
export function setupLocalStoragePort (app) {
  app.ports.writeToLocalStorage.subscribe(function (options) {
    if (options.value !== null) {
      window.localStorage.setItem(options.key, JSON.stringify(options.value, null, 2))
    } else {
      window.localStorage.removeItem(options.key)
    }
  })
}

// eslint-disable-next-line no-unused-vars
export function setupWaterfallPort (app) {
  var update
  function mountOrUpdateWaterfall (data) {
    var containerSelector = '#waterfall'
    var containerElement = document.querySelector(containerSelector)
    if (containerElement) {
      function getViewPort () {
        var aspectRatio = 16 / 9
        return {
          height: containerElement.clientWidth * (1 / aspectRatio),
          width: containerElement.clientWidth
        }
      }
      var svgElement = containerElement.querySelector('svg')
      if (svgElement) {
        if (update) {
          update({
            data: data,
            viewPort: getViewPort()
          })
        }
      } else {
        update = waterfallChart({ // eslint-disable-line no-undef
          containerSelector: containerSelector,
          data: data,
          viewPort: getViewPort()
        })
      }
    }
  }
  app.ports.renderWaterfall.subscribe(function (data) {
    window.onresize = function ( /* event */) {
      mountOrUpdateWaterfall(data)
    }
    // Use requestAnimationFrame to render the chart after the Elm view is rendered.
    requestAnimationFrame(function () {
      mountOrUpdateWaterfall(data)
    })
  })
}

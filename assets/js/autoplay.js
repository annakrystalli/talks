  var slideElements

  function getElementForSlide(slide) {
    slideElements = slideElements || document.querySelectorAll('.remark-slide')
    return slideElements[slide.getSlideIndex()]
  }

  slideshow.on('showSlide', function (slide) {
    Array.from(getElementForSlide(slide).querySelectorAll('video')).forEach(function (vid) {
      vid.loop = true
      vid.currentTime = 0
      vid.play()
    })
  })

  slideshow.on('hideSlide', function (slide) {
    Array.from(getElementForSlide(slide).querySelectorAll('video')).forEach(function (vid) {
      vid.pause()
    })
  })
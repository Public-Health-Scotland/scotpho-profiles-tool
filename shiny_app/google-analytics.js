(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
  a=s.createElement(o), m=s.getElementsByTagName(o)[0];
  a.async=1;
  a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-31372813-3', 'auto');
  ga('set', 'anonymizeIp', true);
  ga('send', 'pageview');

  ////////////////////////// Event tracking /////////////////////// 
  // Tracking clicks to the different tabs. It does track clicks to any links,
  // if no data value assigned they will appear as unset, but this could work to
  // look at all link click with some minor tweaks
     $(document).on('click', 'a', function(e) {
    ga('send', 'event', 'TabsetPanel', 'Tab Viewed', $(this).attr('data-value'));
    
  });
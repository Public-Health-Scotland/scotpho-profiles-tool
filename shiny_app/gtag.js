  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'G-KE1C59RLNS'); 
   
  $(document).on('click', 'a', function(e) {
  gtag('event', 'tab_viewed', {'event_category' : 'TabsetPanel',
    'event_label' : $(this).attr('data-value'),
    'tab_name' : $(this).attr('data-value')
  });
  
}); 


// Track indicators being selected...................

  $(document).on('shiny:inputchanged','#indic_trend', function(f) {
  gtag('event', 'indicator_selected', {'event_category' : 'indicator',
    'event_label' : f.name,
    'indicator_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#indic_rank', function(f) {
  gtag('event', 'indicator_selected', {'event_category' : 'indicator',
    'event_label' : f.name,
    'indicator_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#indic_simd', function(f) {
  gtag('event', 'indicator_selected', {'event_category' : 'indicator',
    'event_label' : f.name,
    'indicator_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#indicator_filter', function(f) {
  gtag('event', 'indicator_selected', {'event_category' : 'indicator',
    'event_label' : "indic_datatab",
    'indicator_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#topic_filter', function(f) {
  gtag('event', 'indicator_selected', {'event_category' : 'topic',
    'event_label' : "indicator_topic_datatab",
    'indicator_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#profile_filter', function(f) {
  gtag('event', 'indicator_selected', {'event_category' : 'profile',
    'event_label' : "indicator_profile_datatab",
    'indicator_name' : f.value
  });
  
});

// Track downloads .......................................

  $(document).on('click', '#download_trend', function(e) {
  gtag('event', 'download', {'event_category' : 'data_download',
    'event_label' : 'download_trend_data'
  });
  
}); 

  $(document).on('click', '#download_trendplot', function(e) {
  gtag('event', 'download', {'event_category' : 'chart_download',
    'event_label' : 'download_trend_chart'
  });
  
}); 

  $(document).on('click', '#download_rank', function(e) {
  gtag('event', 'download', {'event_category' : 'data_download',
    'event_label' : 'download_rank_data'
  });
  
}); 

  $(document).on('click', '#download_rankplot', function(e) {
  gtag('event', 'download', {'event_category' : 'chart_download',
    'event_label' : 'download_rank_chart'
  });
  
});

  $(document).on('click', '#download_mapplot', function(e) {
  gtag('event', 'download', {'event_category' : 'map_download',
    'event_label' : 'download_rank_map'
  });
  
});

  $(document).on('click', '#download_simd', function(e) {
  gtag('event', 'download', {'event_category' : 'data_download',
    'event_label' : 'download_ineq_data'
  });
  
}); 

  $(document).on('click', '#report_simd', function(e) {
  gtag('event', 'download', {'event_category' : 'chart_download',
    'event_label' : 'download_ineq_chart'
  });
  
});

  $(document).on('click', '#download_table_csv', function(e) {
  gtag('event', 'download', {'event_category' : 'data_download',
    'event_label' : 'download_datatab_data'
  });
  
}); 

  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'G-KE1C59RLNS'); 

// Tracks tabs being selected 
   
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

  $(document).on('click', '#download_summary_pdf', function(e) {
  gtag('event', 'download', {'event_category' : 'data_download',
    'event_label' : 'download_summary_pdf'
  });
  
});

  $(document).on('click', '#download_summary_csv', function(e) {
  gtag('event', 'download', {'event_category' : 'data_download',
    'event_label' : 'download_summary_csv'
  });
  
});

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

// Track geographies being selected .........................................

  $(document).on('shiny:inputchanged','#geotype_summary', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "summary_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
}); 

  $(document).on('shiny:inputchanged','#loc_iz_summary', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "summary_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
}); 

  $(document).on('shiny:inputchanged','#hbname_trend', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "trend_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
}); 

  $(document).on('shiny:inputchanged','#partname_trend', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "trend_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#caname_trend', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "trend_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#adpname_trend', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "trend_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#loc_iz_trend', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "trend_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#loc_iz_rank', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "rank_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#geocomp_rank', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "rank_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
});


  $(document).on('shiny:inputchanged','#geotype_simd', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "simd_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#hb_true', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "data_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#la_true', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "data_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#adp_true', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "data_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#hscp_true', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "data_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#hscl_parent', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "data_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#iz_parent', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "data_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
});

  $(document).on('shiny:inputchanged','#code', function(f) {
  gtag('event', 'geography_selected', {'event_category' : "data_tab",
    'event_label' : f.name,
    'geography_name' : f.value
  });
  
});

  $(document).on('click', '#comp_rank1', function(e) {
  gtag('event', 'comparison_selector', {'event_category' : 'rank_tab',
    'event_label' : "area",
    'tab_name' : "area"
  });
  
});

  $(document).on('click', '#comp_rank2', function(e) {
  gtag('event', 'comparison_selector', {'event_category' : 'rank_tab',
    'event_label' : "time",
    'tab_name' : "time"
  });
  
});

  $(document).on('click', '#comp_summary1', function(e) {
  gtag('event', 'comparison_selector', {'event_category' : 'summary_tab',
    'event_label' : "area",
    'tab_name' : "area"
  });
  
});

  $(document).on('click', '#comp_summary2', function(e) {
  gtag('event', 'comparison_selector', {'event_category' : 'summary_tab',
    'event_label' : "time",
    'tab_name' : "time"
  });
  
});


function add_handler(type, who, target){
	$(who).livequery(type, function(){
		self = $(this)
		var loaded = false
		$(target).fadeOut(.5, function(){
			if(!loaded)
				$(this).html('please wait, loading...').fadeIn()
		})

		$.ajax({
			url: $(this).attr('href'),
			dataType: 'html',
			complete: function(res, status){
				loaded = true
				if (status == 'success' || status == 'notmodified'){
					$(target).fadeOut(.5, function(){
						self.parent().children().removeClass('selected')
						self.addClass('selected')
						$(this).html(res.responseText).fadeIn()
					})
				} else {
					$.jGrowl('Error loading data', {theme:  'error'})
					$(target).fadeIn()
				}
			}
		})

		return false
	})
}

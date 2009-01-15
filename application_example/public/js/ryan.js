function add_handler(id, type, target, href, command, fade){
	if(href)
		$(id).attr('href', href)
	
	$(id).bind(type, function(){
		self = $(this)
		if(fade)
			$(target).fadeOut()

		//, function(){
		// 	if(!loaded)
		// 		$(this).html('please wait, loading...').fadeIn()
		// })

		$.ajax({
			url: $(this).attr('href'),
			dataType: 'html',
			complete: function(res, status){
				if (status == 'success' || status == 'notmodified'){
					// self.parent().children().removeClass('selected')
					// self.addClass('selected')
					if(!command || command == 'update')
						$(target).html(res.responseText)
					else if(command == 'append')
						$(target).append(res.responseText)
					else if(command == 'prepend')
						$(target).prepend(res.responseText)
					else if(command == 'empty')
						$(target).empty()
				} else {
					$.jGrowl('Error loading data', {theme:  'error'})
				}

				if(fade)
					$(target).fadeIn()
			}
		})

		return false
	})
}

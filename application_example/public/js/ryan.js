function add_handler(id, type, command, target, argument, fade){
	if(command == 'update' || command == 'append' || command == 'prepend' || command == 'empty' || command == 'message')
		if(argument)
			$(id).attr('rel', argument)
	
	$(id).bind(type, function(){
		if(fade)
			$(target).fadeOut()
		//, function(){
		// 	if(!loaded)
		// 		$(this).html('please wait, loading...').fadeIn()
		// })
		
		if(command == 'update' || command == 'append' || command == 'prepend')
			$.ajax({
				url: $(this).attr('rel'),
				dataType: 'html',
				complete: function(res, status){
					if (status == 'success' || status == 'notmodified'){
						if(command == 'update')
							$(target).html(res.responseText)
						else if(command == 'append')
							$(target).append(res.responseText)
						else if(command == 'prepend')
							$(target).prepend(res.responseText)
						else if(command == 'message')
							$.jGrowl(res.responseText)
					} else {
						$.jGrowl('Error loading data', {theme:  'error'})
					}

					if(fade)
						$(target).fadeIn()
				}
			})
		else if(command == 'empty')
			$(target).empty()
		else if(command == 'addclass')
			$(target).addClass(argument)
		else if(command == 'removeclass')
			$(target).removeClass(argument)
		else if(command == 'toggleclass'){
			$(this).parent().children().removeClass(argument)
			$(this).addClass(argument)
		}

		return false
	})
}

function add_handler(options){
	var id = options['id']
	var event = options['event']
	var command = options['command']
	var what = '#' + options['what']
	var url = options['url']
	var effect = options['effect']
	var clazz = options['clazz']
	var get = options['get']
	
	if(command == 'send' || command == 'update' || command == 'append' || command == 'prepend' || command == 'empty' || command == 'message')
		if(url)
			$(id).attr('rel', 'url')
	
	var on_complete = function(res, status){
		if (status == 'success' || status == 'notmodified'){
			if(effect)
				$(what).hide(effect)
			if(command == 'send')
				$(what).html(res.responseText)
			else if(command == 'update')
				$(what).html(res.responseText)
			else if(command == 'append')
				$(what).append(res.responseText)
			else if(command == 'prepend')
				$(what).prepend(res.responseText)
			else if(command == 'message')
				$.jGrowl(res.responseText)
			if(effect)
				$(what).show(effect)
		} else {
			$.jGrowl('Error loading data: ', {theme:  'error'})
		}
	}

	$(id).bind(event, function(){
		var params = ''
		if(url && get)
			params = '?' + $(get).attr('id') + '=' + $(get).val()

		if(command == 'send' || command == 'update' || command == 'append' || command == 'prepend'){
			if(effect)
				$(what).hide(effect)
			$.ajax({
				url: url + params,
				dataType: 'html',
				complete: on_complete
			})
		} else if(command == 'empty'){
			if(effect)
				$(what).hide(effect, function(){
					$(what).empty()
				})
			else
				$(what).empty()
		}
		else if(command == 'addclass')
			$(what).addClass(clazz)
		else if(command == 'removeclass')
			$(what).removeClass(clazz)
		else if(command == 'toggleclass'){
			$(this).parent().children().removeClass(clazz)
			$(this).addClass(clazz)
		}

		return false
	})
}

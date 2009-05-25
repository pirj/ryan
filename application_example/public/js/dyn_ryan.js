function callback(options){
	var event = options['event']
	var id = options['what']
	var url = options['where']
	var get = options['get']

	var on_complete = function(reply){
		parse_reply(reply)
	}

	
	$(id).bind(event, function(){
		var data = {}
		if(get)
			data[$(get).attr('id')] = $(get).val()
		else
			data['_id'] = $(id).attr('_id')
		$.getJSON(url, data, on_complete)
	})
}

function parse_reply(reply){
	for(i in reply){
		reaction = reactions[reply[i]['command']]
		reaction(reply[i])
	}
}

var reactions = {
	growl: growl,
	hide: hide,
	empty: empty,
	prepend: prepend,
	update: update
}

function growl(data){
	$.jGrowl(data['text'])
}

function hide(data){
	$(data['where']).hide(data['effect'])
}

function empty(data){
	$(data['where']).empty(data['effect'])
}

function update(data){
	$(data['where']).html(data['html'])
	if(data['effect'])
		$(data['where']).show(data['effect'])
}

function prepend(data){
	$(data['where']).prepend(data['html'])
	if(data['effect'])
		$(data['where']).show(data['effect'])
}

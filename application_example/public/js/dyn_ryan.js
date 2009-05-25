function callback(options){
	var event = options['event']
	var id = options['what']
	var url = options['where']

	var on_complete = function(reply){
		parse_reply(reply)
	}

	$(id).bind(event, function(){
		$.getJSON(url, on_complete)
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
	update: update
}

function growl(data){
	$.jGrowl(data['text'])
}

function update(data){
	$(data['where']).html(data['html'])
	if(data['effect'])
		$(data['where']).show(data['effect'])
}

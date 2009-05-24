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
	for(i in reply)
		for(j in reply[i]){
			reaction = reactions[j]
			reaction(reply[i][j])
		}
}

var reactions = {
	growl: growl,
	update: update
}

function growl(data){
	$.jGrowl(data)
}

function update(data){
	if(data['text']){
		$(data['where']).html(data['text'])
		if(data['effect'])
			$(data['where']).show(data['effect'])
	}
	if(data['url'])
		$.get(data['url'], function(reply){
			$(data['where']).html(reply)
			if(data['effect'])
				$(data['where']).show(data['effect'])
		})
}

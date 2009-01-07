$(document).ready(function() {

	$('div#apples a').livequery('click',function(){
		self = $(this)
		$.ajax({
			url: $(this).attr('href'),
			dataType: 'html',
			complete: function(res, status){
				if (status == 'success' || status == 'notmodified'){
					$('#apple').fadeOut(.5, function(){
						self.parent().children().removeClass('selected')
						self.addClass('selected')
						$(this).html(res.responseText).fadeIn()
					})
				} else {
					var error = 'Error loading data'
					$.jGrowl(error, {theme:  'error'})
					$('#apple').fadeOut(.5, function(){
						$(this).html(error).fadeIn()
					})
				}
			}
		})

		return false
	})

	$('div#menu li').livequery('click',function(){
		self = $(this)
		var loaded = false
		$('#contents').fadeOut(.5, function(){
			if(!loaded)
				$(this).html('please wait, loading...').fadeIn()
		})

		$.ajax({
			url: $(this).children('a').attr('href'),
			dataType: 'html',
			complete: function(res, status){
				loaded = true
				if (status == 'success' || status == 'notmodified'){
					$('#contents').fadeOut(.5, function(){
						self.parent().children().removeClass('selected')
						self.addClass('selected')
						$(this).html(res.responseText).fadeIn()
					})
				} else {
					var error = 'Error loading data'
					$.jGrowl(error, {theme:  'error'})
					$('#contents').fadeOut(.5, function(){
						$(this).html(error).fadeIn()
					})
				}
			}
		})

		return false
	})

})

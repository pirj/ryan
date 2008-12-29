$(document).ready(function() {

	$('div.apples a').livequery('click',function(){
		var loaded = false
		$('.apple').fadeOut(.5, function(){
			if(!loaded)
				$(this).html('please wait, loading...').fadeIn()
		})

		$.ajax({
			url: $(this).attr('href'),
			dataType: 'html',
			complete: function(res, status){
				loaded = true
				if (status == 'success' || status == 'notmodified'){
					$('.apple').fadeOut(.5, function(){
						$(this).html(res.responseText).fadeIn()
					})
				} else
					$.jGrowl('error', {theme:  'error'})
			}
		})

		return false
	})
})

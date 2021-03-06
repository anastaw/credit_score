$(function(){
	//Handler for basic RPC
	$("#scorebutton").click(function(e){
		e.preventDefault()
		$(".tvfield").val("")
		var data = [];
		$("tbody tr").each(function(i){
			data[i] = {
				{{Duration.in.month}} : parseFloat($(this).find(".lengthfield").val()),
				{{Credit.amount}} : parseFloat($(this).find(".amountfield").val()),
				{{Installment.rate.in.percentage.of.disposable.income}}: 0,
				{{Status.of.existing.checking.accountA13}}: 0,
				{{Status.of.existing.checking.accountA14}}: 0,
				{{Credit.historyA32}}: 0,
				{{Credit.historyA33}}: 0,
				{{Credit.historyA34}}: 0,
				{{PurposeA41}}: 0,
				{{Savings.account.bondsA64}}: 0,
				{{Savings.account.bondsA65}}: 0,
				{{Present.employment.since.A74}}: 0,
				{{Other.debtors...guarantorsA103}}: 0,
				{{Other.installment.plansA143}}: 0,
				{{Housing.A15}}: 0,	
				{{Housing.A152}}: 0
			};
		});
		
		//RPC request to score data
		var req = ocpu.rpc("credit", {input : data}, function(output){
			//repopulate the table
			$("tbody tr").each(function(i){
				$(this).find(".lengthfield").val(output[i].Duration.in.month);
				$(this).find(".amountfield").val(output[i].Credit.amount);
				$(this).find(".tvfield").val(output[i].credit.score);
			});
		}).fail(function(){
			alert(req.responseText);
		});
	});

	//CSV file scoring
	$("#csvfile").on("change", function loadfile(e){
		if(!$("#csvfile").val()) return;
		$("#outputcsv").addClass("hide").attr("href", "");
		$(".spinner").show()
		var req = ocpu.call("tv", {
			input : $("#csvfile")[0].files[0]
		}, function(tmp){
			$("#outputcsv").removeClass("hide").attr("href", tmp.getLoc() + "R/.val/csv")
		}).fail(function(){
			alert(req.responseText)
		}).always(function(){
			$(".spinner").hide()
		});
	});

	//update the example curl line with the current server
	$("#curlcode").text(
		$("#curlcode").text().replace(
			"https://public.opencpu.org/ocpu/github/opencpu/tvscore/R/tv/json", 
			window.location.href.match(".*/credit/")[0] + "R/tv/json"
		)
	);

	//this is just to create a table
	function addrow(){
		$("tbody").append('<tr> <td> <div class="form-group"> <input type="number" min="20" max="80" class="form-control durationfield" placeholder="Duration"> </div> </td> <td> <div class="form-group"> <input type="number" min="20" max="80" class="form-control amountfield" placeholder="Amount"> </div> </td> <td> <div class="form-group"> <input disabled="disabled" class="disabled form-control tvfield"> </div> </td> </tr>');
	}

	for(var i = 0; i < 5; i++){
		addrow();
	}
});


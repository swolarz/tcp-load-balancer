
var monitorChart;
var datasets = [
];

const setupMonitoringChart = () => {
	var monitorChartCtx = document.getElementById("monitorChartCanvas").getContext('2d');
	
	monitorChart = new Chart(monitorChartCtx, {
		type: 'horizontalBar',
		options: {
			elements: {
				rectangle: {
					borderWidth: 2
				}
			},
			scales: {
				xAxes: [{
					ticks: {
						beginAtZero: true
					}
				}],
				yAxes: [{
					ticks: {
						display: false
					}
				}]
			},
			responsive: true,
			legend: {
				position: 'right'
			},
			title: {
				display: true,
				text: 'Served requests by each instance'
			}
		},
		data: {
			datasets: [
			]
		}
	});

	return;
};

export { setupMonitoringChart };


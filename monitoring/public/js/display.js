
const minScaleMaxValue = 100;
var scaleMax = minScaleMaxValue;
var barColors = [
	'rgb(255, 99, 132)',		// red
	'rgb(255, 159, 64)',	// orange
	'rgb(255, 205, 86)',	// yellow
	'rgb(75, 192, 192)',		// green
	'rgb(54, 162, 235)',		// blue
	'rgb(153, 102, 255)',	// purple
	'rgb(201, 203, 207)'		// grey
];
var nextColor = 0;
var monitorChart;


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
						beginAtZero: true,
						suggestedMax: minScaleMaxValue,
						stepSize: 1
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
			datasets: []
		}
	});
};


const barFillName = (bar, name) => {
	bar._id = name;
	return bar;
};

const getNextColor = () => {
	let color = barColors[nextColor];
	nextColor = (nextColor + 1) % barColors.length;

	return Color(color);
};

const barFillStyle = (bar) => {
	let color = getNextColor();

	bar.backgroundColor = color.alpha(0.5).rgbString();
	bar.borderColor = color;

	return bar;
};

const barFillData = (bar, name, value) => {
	bar.label = name;
	bar.data = [value];

	return bar;
};

const makeBar = (name, value) => {
	return barFillData(
		barFillStyle(
			barFillName({}, name)
		),
		name, value
	);
};


const updateScale = () => {
	let barData = monitorChart.data.datasets;
	let dataScaleMax = barData.map(bar => bar.data[0]).reduce((a, b) => Math.max(a, b), 0);

	if (dataScaleMax > scaleMax * 0.9) {
		scaleMax = scaleMax * 10;
	}
	else if (dataScaleMax < scaleMax * 0.05) {
		scaleMax = Math.round(scaleMax / 10);
	}

	scaleMax = Math.max(scaleMax, minScaleMaxValue);
	monitorChart.options.scales.xAxes[0].ticks.suggestedMax = scaleMax;
};

const updateChart = () => {
	updateScale();
	monitorChart.update();
};


const addBar = (name, value) => {
	let bar = makeBar(name, value);
	let barData = monitorChart.data.datasets;

	barData.push(bar);
	updateChart();
};

const removeBar = (name) => {
	let barData = monitorChart.data.datasets;
	let filteredData = barData.filter((bar) => bar._id !== name);

	monitorChart.data.datasets = filteredData;
	updateChart();
};

const updateBar = (name, value) => {
	let barData = monitorChart.data.datasets;

	barData.filter((bar) => bar._id === name).forEach((bar) => bar.data = [value]);
	updateChart();
};


export { setupMonitoringChart, addBar, removeBar, updateBar };



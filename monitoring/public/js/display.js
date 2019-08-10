
var monitorChart;

var nextId = 1;

var barColors = {
	red: 'rgb(255, 99, 132)',
	orange: 'rgb(255, 159, 64)',
	yellow: 'rgb(255, 205, 86)',
	green: 'rgb(75, 192, 192)',
	blue: 'rgb(54, 162, 235)',
	purple: 'rgb(153, 102, 255)',
	grey: 'rgb(201, 203, 207)'
};
var nextColor = 0;

const minScaleMaxValue = 10;


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


const barFillId = (bar) => {
	bar._id = nextId++;
	return bar;
};

const getNextColor = () => {
	let color = barColors[nextColor];
	nextColor = (nextColor + 1) % barColors.length;

	return color;
};

const barFillStyle = (bar) => {
	let color = getNextColor();

	bar.backgroundColor = color.alpha(0.5).rgbString();
	bar.borderColor = color;

	return bar;
};

const barFillData = (bar, name, value) => {
	bar.label = label;
	bar.data = [value];

	return bar;
};

const makeBar = (name, value) => {
	return barFillData(
		barFillStyle(
			barFillId({})
		),
		name, value
	);
};


const updateScale = () => {
	let barData = monitorChart.data.datasets;
	let currentScaleMax = monitorChart.options.scales.xAxes.ticks.suggestedMax;
	let dataScaleMax = barData.map(bar => bar.data[0]).reduce((a, b) => Math.max(a, b));

	if (dataScaleMax > currentScaleMax * 0.8) {
		dataScaleMax = currentScaleMax * 2;
	}
	else if (dataScaleMax < currentScaleMax * 0.5) {
		dataScaleMax = Math.round(currentScaleMax / 2);
	}

	dataScaleMax = Math.max(dataScaleMax, minScaleMaxValue);
	monitorChart.options.scales.xAxes.ticks.suggestedMax = dataScaleMax;
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

	return bar._id;
};

const removeBar = (id) => {
	let barData = monitorChart.data.datasets;
	let filteredData = barData.filter((bar) => bar._id !== id);

	monitorChart.data.datasets = filteredData;
	updateChart();
};

const updateBar = (id, value) => {
	let barData = monitorChart.data.datasets;

	barData.filter((bar) => bar._id === id).forEach((bar) => bar.data = [value]);
	updateChart();
};


export { setupMonitoringChart, addBar, removeBar, updateBar };



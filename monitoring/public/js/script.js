import * as display from './display.js';
import { setupListener } from './status.js';


window.onload = () => {
	display.setupMonitoringChart();

	callbacks = {
		instanceUp: (msg) => {},
		instanceDown: (msg) => {},
		instanceUpdate: (msg) => {}
	};

	setupListener(callbacks);
};


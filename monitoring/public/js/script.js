import * as display from './display.js';
import { setupListener } from './status.js';


window.onload = () => {
	display.setupMonitoringChart();

	let callbacks = {
		instanceUp: (msg) => {
			display.addBar(msg.name, msg.cnt);
		},
		instanceDown: (msg) => {
			display.removeBar(msg.name);
		},
		instanceUpdate: (msg) => {
			display.updateBar(msg.name, msg.cnt);
		}
	};

	setupListener(callbacks);
};


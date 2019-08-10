import * as display from './display.js';
import { setupListener } from './status.js';


window.onload = () => {
	display.setupMonitoringChart();

	let callbacks = {
		instanceUp: (msg) => {
			return display.addBar(msg.name, msg.cnt);
		},
		instanceDown: (msg) => {
			removeBar(msg.id);
		},
		instanceUpdate: (msg) => {
			updateBar(msg.id, msg.cnt);
		}
	};

	setupListener(callbacks);
};


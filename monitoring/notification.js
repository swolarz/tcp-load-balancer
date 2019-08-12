var subscribers = [];
var instances = {};


const notifyChangedInstances = (changedInstanceNames, targets, create = false) => {
	let msgType = (create === true ? 'up' : 'updt');

	changedInstanceNames.forEach((instanceName) => {
		let instance = instances[instanceName];

		targets.forEach((subscriber) => {
			let msg = [msgType, { name: instanceName, cnt: instance.reqs }];
			subscriber.ws.send(JSON.stringify(msg));
		});
	});
};

const notifyRemovedInstances = (removedInstanceNames, targets) => {
	let msgType = 'down';

	removedInstanceNames.forEach((instance) => {
		targets.forEach((subscriber) => {
			let msg = [msgType, { name: instance }];
			subscriber.ws.send(JSON.stringify(msg));
		});
	});
};

const handleInstanceUpdate = (instance, requests) => {
	console.log(`Updating instance [${instance}] requests number: ${requests}`);

	instances[instance].reqs = requests;
	notifyChangedInstances([instance], subscribers);
};

const registerSubscriber = (ws) => {
	let subscriber = { ws: ws };
	subscribers.push(subscriber);

	notifyChangedInstances(Object.keys(instances), [subscriber], true);
};

const registerInstance = (instance) => {
	instances[instance] = {
		name: instance,
		reqs: 0
	};

	notifyChangedInstances([instance], subscribers, true);
};

const unregisterInstance = (instance) => {
	delete subscribers[instance];
	notifyRemovedInstances([instance], subscribers);
};

const instanceExists = (instance) => {
	return subscribers.hasOwnProperty(instance);
};

const statusEndpointHandler = (ws, req) => {
	try {
		console.log('Received monitoring client connection');
		registerSubscriber(ws);
	}
	catch (ex) {
		console.log(ex);
	}
};

const listenerEndpointHandler = (ws, req) => {
	try {
		console.log('Received connection from a service instance:', req.query.instance);
		let instanceName = req.query.instance;

		if (!instanceName || instanceExists(instanceName)) {
			console.log('Instance already exists or name is not specified. Ignoring request');
			ws.close();
			return;
		}

		registerInstance(instanceName);

		ws.on('message', msg => {
			try {
				msg = JSON.parse(msg);
				let requests = msg.reqs;

				if (requests) {
					handleInstanceUpdate(instanceName, requests);
				}
			}
			catch (ex) {
				console.log(ex);
			}
		});

		ws.on('close', msg => {
			try {
				unregisterInstance(instanceName);
			}
			catch (ex) {
				console.log(ex);
			}
		});
	}
	catch (ex) {
		console.log(ex);
	}
};


module.exports = {
	status: statusEndpointHandler,
	listener: listenerEndpointHandler
};


var subscribers = {};

const registerSubscriber = (ws) => {
	let subscriber = {
		displayId: -1
	};

	ws.on('message', (msg) => {
		msg = JSON.parse(msg);

		if (msg.type === 'display-init') {
			subscriber.displayId = msg.displayId;
		}
	});
};

const 

const handleInstanceUpdate = (instance, requests) => {
	console.log(`Updating instance [${instance}] requests number: ${requests}`);
};

const registerInstance = (instance) => {
	subscribers[instance] = {
		reqs: 0
	};
};

const unregisterInstance = (instance) => {
	delete subscribers[instance];
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
	}
	catch (ex) {
		console.log(ex);
	}
};


module.exports = {
	status: statusEndpointHandler,
	listener: listenerEndpointHandler
};


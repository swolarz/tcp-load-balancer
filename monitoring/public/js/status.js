
const statusWebsockUrl = () => 'ws://' + document.location.host + '/status';

const prepUpMsg = (upMsg) => {
	return upMsg;
};

const prepDownMsg = (downMsg) => {
	return downMsg;
};

const prepReqMsg = (reqMsg) => {
	return prepReqMsg;
};

const handleStatusMessage = (msgEvent, callbacks) => {
	let msgJson = JSON.parse(msgEvent.data);
	let [msgType, msg] = msgJson;

	if (msgType === 'up') {
		callbacks.instanceUp(prepUpMsg(msg));
	}
	else if (msgType === 'down') {
		callbacks.instanceDown(prepDownMsg(msg));
	}
	else if (msgType === 'req') {
		callbacks.instanceUpdate(prepReqMsg(msg));
	}
	else {
		console.log(`Err: unknown message: ${msgJson}`);
	}
};

const setupListener = (callbacks) => {
	var statusWebsock = new WebSocket(statusWebsockUrl());

	statusWebsock.onmessage = (msgEvent) => handleStatusMessage(msgEvent, callbacks);
};

export { setupListener };

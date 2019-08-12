
const statusWebsockUrl = () => 'ws://' + document.location.host + '/status';

const prepUpMsg = (upMsg) => upMsg;
const prepDownMsg = (downMsg) => downMsg;
const prepReqMsg = (reqMsg) => reqMsg;

const handleStatusMessage = (ws, msgEvent, callbacks) => {
	let msgJson = JSON.parse(msgEvent.data);
	let [msgType, msg] = msgJson;

	if (msgType === 'up') {
		callbacks.instanceUp(prepUpMsg(msg));
	}
	else if (msgType === 'down') {
		callbacks.instanceDown(prepDownMsg(msg));
	}
	else if (msgType === 'updt') {
		callbacks.instanceUpdate(prepReqMsg(msg));
	}
	else {
		console.log(`Err: unknown message: ${msgJson}`);
	}
};

const setupListener = (callbacks) => {
	var statusWebsock = new WebSocket(statusWebsockUrl());
	statusWebsock.onmessage = (msgEvent) => handleStatusMessage(statusWebsock, msgEvent, callbacks);
};

export { setupListener };


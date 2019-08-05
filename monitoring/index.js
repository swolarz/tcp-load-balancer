const express = require('express');
const websocket = require('express-ws');
const path = require('path');

const config = require('./config.json');


const app = express();
websocket(app);


app.use(express.static(path.join(__dirname, 'public')));

app.ws("/status", (ws, req) => {
	console.log('Received websocket connection');

	ws.on('message', msg => {
		console.log('Received message:', JSON.stringify(msg));
	});
});


const serverPort = process.env.PORT || config.server.port || 8080;

app.listen(serverPort, () => {
	console.log(`Server started on port ${serverPort}`);
});

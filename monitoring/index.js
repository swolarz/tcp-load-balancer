const express = require('express');
const websocket = require('express-ws');
const path = require('path');

const config = require('./config.json');
const notificationRouter = require('./notification.js');


const app = express();
websocket(app);

app.use(express.static(path.join(__dirname, 'public')));
app.ws('/status', notificationRouter.status);
app.ws('/listener', notificationRouter.listener);

const serverPort = process.env.PORT || config.server.port || 8080;

app.listen(serverPort, () => {
	console.log(`Server started on port ${serverPort}`);
});


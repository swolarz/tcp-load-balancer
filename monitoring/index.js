const express = require('express');
const path = require('path');

const config = require('./config.json');


const app = express();
const serverPort = process.env.PORT || config.server.port || 5000;


app.use(express.static(path.join(__dirname, 'public')));

app.listen(serverPort, () => {
	console.log(`Server started on port ${serverPort}`);
});

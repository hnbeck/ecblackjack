///////////////////////////////////////////////////////////////////////////////
//
// costumeUI.js
//
// contains all code for implementing the costumes and interaction
//
// (c) Hans N. Beck
///////////////////////////////////////////////////////////////////////////////

// the Prolog engines
var pengine; 
var session = pl.create(1000);
var result; // for communication with Prolog

// the objects, which are costumes
var lights; 
var aDesk; 
var gameCanvas; 
var costumes; 
var aDeskFrame; 
var aMessage; 
var winSound; 
var loseSound; 
var cardDrawSound; 
var cardSetSound; 

// useful constants
// index constants
const cX = 1;
const cY = 2; 
const cWidth = 3; 
const cHeight = 4; 
const cMargin = 5; 
// basic data
const cMidMargin = 6; 
const canvasW = 800; 
const canvasH = 350; 
const player1 = 1; 
const player2 = 2; 
const OTranslator = {up:true, down:false}; //Orientationtranslator

// geometric functions
// calculate a card frame relative to desk frame
// placeNo is zero based
function cardFrame(placeNo, playerNo, deskFrame) {
	var frame = new Array(7);
	// because playerno are 1-based
	// placeNo is 0-based
	var effectivePlayerNo = playerNo - 1; 
	
	// calculate frame
	frame[cWidth] = deskFrame[cWidth]/8; // width
	frame[cHeight] = frame[cWidth] * 1.5; // height
	frame[cMargin] = 5; // margin
	frame[cX] = deskFrame[cMargin] + frame[cMargin];
	frame[cY] =	deskFrame[cMargin] + frame[cMargin]; 
		// positioning
	frame[cX] = frame[cX] + placeNo*(frame[cWidth] + frame[cMargin]);
	frame[cY] = frame[cY] + effectivePlayerNo*(frame[cHeight] + frame[cMargin]);
			
	return frame; 
}
// calculate a desk frame relative to window size
function deskFrame(W, H) {
	var frame = new Array(7);

	frame[cWidth] = W; // width
	frame[cHeight] = H; // height
	frame[cMargin] = floor(W / 800*16); // margin
	frame[cMidMargin] = frame[cMargin]/2;  // position mid of border
	
	return frame; 
}

///////////////////////////// Costumes /////////////////////////////////

// the message costume
class Message  {
	constructor(text,  frameFkt, deskFrame) {
		this.text = text; 
		this.frame = frameFkt(0, 1, deskFrame); // per def the position of text
	}
	
	draw() {
		textSize(20);
		textAlign(CENTER, CENTER)
		text(this.text, 
				this.frame[cX], 
				this.frame[cY],
				this.frame[cWidth], 
				this.frame[cHeight]);
		}
	
	message(text) {
		this.text = text; 
	}
}

// Costume for a lamp
class Lamp {
	constructor(x, y, c, frame) {
		this.color = c; 
		this.x = x; 
		this.y = y; 
		this.diameter = frame[cMargin]; 
		this.lumPhase = 0; 
		this.lichtprofil = new Array(51);
		// calculate luminance data for flashing
		this.lichtprofil[25] = 50; 
		for (var i = 1; i < 26; i++) {
			this.lichtprofil[25-i] = 50 - (i);
			this.lichtprofil[25+i] = 50 - (i);
		}
	};
	// set the luminance
	luminate(){
		var value = this.fadeLuminance();
		var hVal = hue(this.color);
		var sVal = saturation(this.color);
		this.color = color(hVal, sVal, value);
	};
	// luminance change
	fadeLuminance() {
		var value = 25; 

		this.lumPhase += 1; 
		if ((this.lumPhase  >= 0) && (this.lumPhase < this.lichtprofil.length)) {
			value = this.lichtprofil[this.lumPhase];
		} else {
			this.lumPhase = 0; 
		}
		return value; 
	};

	draw() {
		noStroke(); 
		fill(this.color);
		circle(this.x,this.y,this.diameter);
	};
}

// costume for desk
class Desk {
	constructor(frame) {
		this.frame = frame; 
		this.lampDia = frame[cMargin];
		colorMode(HSL, 360,100,100,1); 
	}

	draw() {
		let borderCol = color(35,79, 23);
		let deskCol = color(135,50,21);

		noStroke(); 
		fill(borderCol);
		rect(0, 0, this.frame[cWidth], this.frame[cHeight]);
		noStroke(); 
		fill(deskCol);
		rect(	this.lampDia,
				this.lampDia,
				this.frame[cWidth]-this.lampDia-this.lampDia,
				this.frame[cHeight]-this.lampDia-this.lampDia
			);
		}
}

// costume for the lamps around the desk
class LampLine {
	constructor(frame) {
		var deskWidth = frame[cWidth];
		var deskHeight = frame[cHeight];
		var lampDia = frame[cMargin]; // lamps diameter is same as border thickness
		var firstLampX = frame[cMidMargin]; 
		var firstLampY = firstLampX; // Border has in every direction the same thickness
		var lastPosX; 
		var lastPosY; 

		this.lamps = [];

		colorMode(HSL, 360,100,100,1); 
		let col = color(60, 100, 25);

		// generate the lamps around the desk
		for (var ix = firstLampX;  ix <= deskWidth; ix += lampDia) {
			this.lamps.push(new Lamp(ix, firstLampY, col, frame));
			lastPosX = ix; 
		}
		for (var iy = firstLampY-lampDia; iy <= deskHeight - lampDia; iy += lampDia){
			this.lamps.push(new Lamp(lastPosX, iy, col, frame));
			lastPosY = iy;
		}
		for (ix = lastPosX; ix >= firstLampX;  ix -= lampDia){
			this.lamps.push(new Lamp(ix, firstLampY+(deskHeight-lampDia), col, frame));
		}
		for (iy = lastPosY; iy >= firstLampY-lampDia;  iy -= lampDia){
			this.lamps.push(new Lamp(firstLampX, iy, col, frame));
		}
	}

	draw() {
		for (var i = 0; i< this.lamps.length; i++)
		{
			this.lamps[i].luminate();
			this.lamps[i].draw();
		}
	}
}

// costume of a card
class Costume {
	constructor(cardName, cardNo, playerNo, frameFkt, deskFrame) {
		const noSteps = 20;
		this.cardFrame = frameFkt(cardNo, playerNo, deskFrame);
		this.sourceFrame = frameFkt(0, 2, deskFrame); // place of the first card
		this.imgUp = loadImage('/graphics/'+cardName+'.png');
		this.imgDown = loadImage('/graphics/backside.png');
		this.owner = playerNo;  
		this.name = cardName; 
		this.inactive = false; 
		this.upFlag = true; 
		this.angle = 0; 
		// geometric data needed for displacement and animation
		var deltaX =  this.cardFrame[cX] - this.sourceFrame[cX];
		var deltaY =  this.cardFrame[cY] - this.sourceFrame[cY];
		this.slope =  deltaY / deltaX;
		this.step = deltaX / noSteps; 
		this.posX = this.sourceFrame[cX];
		this.posY = this.sourceFrame[cY];
		this.deltaAngle = 360/noSteps; 
		// play the sound, delay is needed so that the sounds doesnt interfere
		if (cardNo > 0) {
			if (clickSound.isPlaying()) {
				setTimeout(function(){cardDrawSound.play();},600);
			} else {
				cardDrawSound.play(); 
			}
		}
		//console.log('costume name is: ' + cardName);
		//console.log('costume cX ' + this.cardFrame[cX] + ' cy ' + this.cardFrame[cY]);
		//console.log("Costume initialization done");
		//console.log('Slope ', this.slope);
	}

	draw() {
		push();
	
		// card is moving and not at the destination place
		if (abs(this.posX - this.cardFrame[cX]) > 0.01) {
			this.posX = this.posX + this.step; 
			this.posY = this.posY + this.slope*this.step;
			this.angle += this.deltaAngle;
		}
		else {
			// card is at destination
			if (this.deltaAngle != 0) {
				cardSetSound.play(); 
				this.deltaAngle = 0; 
			}
		}
		
		translate(this.posX+this.cardFrame[cWidth]/2,this.posY +this.cardFrame[cHeight]/2);
		rotate(this.angle);
		translate(-this.cardFrame[cWidth]/2, -this.cardFrame[cHeight]/2);
		
		if (this.upFlag == true){
			image(this.imgUp, 
				0, 
				0, 
				this.cardFrame[cWidth],
				this.cardFrame[cHeight]);
		}
		else {
			image(this.imgDown, 
				0, 
				0, 
				this.cardFrame[cWidth],
				this.cardFrame[cHeight]);
		}
		pop(); 
	}
	// click sensor
	touch(aX, aY ){
		var retVal = false; 

		if (!this.inactive)
			if (aX > this.cardFrame[cX]) 
				if (aX < this.cardFrame[cX]+this.cardFrame[cWidth])
					if (aY > this.cardFrame[cY])
						if (aY < this.cardFrame[cY]+this.cardFrame[cHeight])
							retVal = true; 

		return retVal; 
	}

	deactivate(){
		this.inactive = true; 
	}
	// card can be up or down
	orientation(OrientationRequest){
		this.upFlag = OTranslator[OrientationRequest];
	}
}

		
////////////////////////// Prolog Code ///////////////////////

// setup Prolog system
function init_Prolog() {
	// load tau
	$.get("/web/webProlog.pl", function(data) {
		session.consult(data);
 	});

	// Tau is loaded, now pengine and during this also init tau
	pengine = new Pengine({
		oncreate: handleCreate, 
		onsuccess: handleOutput,
		destroy: false
	}); 	

	console.log('Prolog Init done');
}
		
// here are the Pengine handle functions
function handleCreate() {
	// init tau prolog
	session.query("init.");
	session.answer(printAnswer);
	$("#Tauhtml").text('');
	// call the init of the game at SWI Prolog
	pengine.ask('createGame(P1, P2, Flag, Msg)');
}
// Pengine handle function for reveiving SWI Prolog answer
function handleOutput(){
	var resList = []; 

	for (x in this.data[0]){
		// properties must be lowercase in order for Tau Prolog
		this.data[0][x.toLowerCase()] = this.data[0][x];
		this.data[0][x].delete;
		resList.push(x.toLowerCase());
	}
	// store the answer of Pengine (=JSON object) into global variable 
	result = this.data[0];
	//console.log(result);
	
	// analyse the result taken from Pengine. This will result in updated
	// knowledge base (=new predicates) in Tau Prolog
	session.query("takeResult(["+ resList.toString() + "], result, Term).");
	session.answer(printAnswer);
	// process the next game state
	session.query("gameContinue.");
	session.answer(printAnswer);
}

/////// Tau Prolog ///////

// Callback needed for triggering and displaying answers of Tau prolog querys
var printAnswer = function(answer){
	// Debug code
	//$("#Tauout").append(pl.format_answer(answer));
	//$("#Tauout").append("<br>");
	//console.log(pl.query, ' Tau answer:' + answer);
}
// send a query to pengine = SWI Prolog = Sever Code
function sendPengine() {
	var query = $("#Tauhtml").text();
	//console.log("Query will be: " + query);
	pengine.ask(query);
}

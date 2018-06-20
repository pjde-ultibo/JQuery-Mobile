/*
   Test Progrm for WebSockets
*/

var socket = null;
var uws = new Boolean ();		// use ws
var WSParams = "";

function decode (xml) // decode the response from the server 
{
	var elems;
	var elem;
	
	var xmlDoc = $.parseXML (xml);
	if (xmlDoc)
	{
		elems = $(xmlDoc).find ("value");
		$(elems).each (function (i)
		{
			elem = $(this).attr ("id");
			if (elem != undefined)
			{
				$("#" + elem).attr ("value", $(this).attr ("value")); 	// set value of slider
				$("#" + elem).slider ('refresh');						// refresh slider
			} 
		});
	} 
}
				
function ValueChanged (id, value)
{
	if (uws) socket.send ('<value id="' + id + '" value="' + value + '"/>\r');
}

function WSConnect ()
{
	if ("MozWebSocket" in window)
	{
		socket = new MozWebSocket ("ws://" + WSParams + "/websocket", "klingon");  
	}
	else if ("WebSocket" in window)
	{
		socket = new WebSocket ("ws://" + WSParams + "/websocket", "klingon");  
	}
	else
	{
		alert ("Web Sockets not supported by this Browser.");
		return;
	}
	socket.onopen = function (evt)
	{  
		uws = true;									// using ws
	}  
    socket.onmessage = function (msg)
	{  
		decode (msg.data);
	}  
    socket.onclose = function (evt)
	{
		uws = false;
	}
	socket.onerror = function (evt)
	{
	}
}

function InitSlider ()
{
	$("#sdr1").change (function () 				// volume slider
	{
		ValueChanged ($(this).attr ("id"), $(this).attr ("value"));
	});
} 

function Init (params)
{
	uws = false;
	WSParams = params;
	WSConnect ();		// connect to Pi via WebSocket
	InitSlider ();		// initialise the OnChange event on the slider 
}


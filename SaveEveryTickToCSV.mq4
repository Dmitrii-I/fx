// import the WINAPI GetLocalTime function from the kernel32.dll
#import "kernel32.dll"
void GetLocalTime(int& TimeArray[]); // the function from kernel32.dll to be used to retrieve time
#import

string TimeInMillisec() {
// Returns timestamp with millisecond resolution
   
   int TimeArray[4]; // this array will hold the SYSTEMTIME struct
   GetLocalTime(TimeArray); // 32 bit per element, 128 bits total
   
   int nYear, nMonth, nDay, nHour, nMin, nSec, nMillisec;
   // parse the SYSTEMTIME struct using ">>" and "&" bitwise operators
   nYear=TimeArray[0]&0x0000FFFF;
   nMonth=TimeArray[0]>>16;
   nDay=TimeArray[1]>>16;
   nHour=(TimeArray[2]&0x0000FFFF);
   nMin=TimeArray[2]>>16;
   nSec=TimeArray[3]&0x0000FFFF;
   nMillisec=TimeArray[3]>>16; // note that leading zeroes will be stripped off 016 will become 16
   
   // Use strings to construct timestamp in order to have leading zeroes
   string strYear, strMonth, strDay, strHour, strMin, strSec, strMillisec;
   strYear = nYear; // no leading zeroes for year needed for coming 7898 years
   
   if (nMonth < 10) strMonth = "0" + nMonth; 
   else strMonth = nMonth;
   
   if (nDay < 10) strDay = "0" + nDay; 
   else strDay = nDay;
   
   if (nHour < 10) strHour = "0" + nHour; 
   else strHour = nHour;
   
   if (nMin < 10) strMin = "0" + nMin; 
   else strMin = nMin;
   
   if (nSec < 10) strSec = "0" + nSec; 
   else strSec = nSec;
   
   if (nMillisec < 10) {
      strMillisec = "00" + nMillisec; 
   } else if  (nMillisec < 100) {
      strMillisec = "0" + nMillisec;
   } else {
      strMillisec = nMillisec;
   }

  // Construct the timestamp string 
   string localTime = strYear + "-" + strMonth + "-" + strDay + " " + strHour + ":" + strMin + ":" + strSec + "." + strMillisec;

   return(localTime);
}

string ReplaceSpaces(string terminalName) {
   while (StringFind(terminalName, " ", 0) != -1) {
      Text = StringSetChar(terminalName, StringFind(terminalName, " ", 0), '-');
   }
   return(terminalName); 
}

int fileHandle; // Global because init() and deinit() need to call it. Contains the handle to the csv file

// This function is executed when the expert advisor is loaded
int init() {
   string fileName = StringConcatenate(Symbol(), "-", ReplaceSpaces(TerminalName()), ".csv"); // the name of the file where to store the tick data
   fileHandle=FileOpen(fileName,FILE_CSV|FILE_WRITE,","); // create reference to the file
   if(fileHandle<1) { // if we failed to open or create the file
      Print("File ", fileName, " not found"); // prints message to the experts log
   } 
   else {
      Print("File ", fileName, " is opened. Its handle is ", fileHandle); 
   }   
   return(0);
}
//---------------------------------------------------------------------------


// this function is executed when we stop the expert advisor
int deinit() {
   if(fileHandle>0) {
      FileClose(fileHandle);
      Print("File with file handle ", fileHandle, " is closed");
   }
   return(0);
}
//---------------------------------------------------------------------------

// the start function is executed every time a tick arrives
int start() {
   // string timestamp = StringConcatenate(Day(),"/",Month(),"/",Year()," ",Hour(),":",Minute(),":",Seconds()); 
   string timestamp = TimeInMillisec(); // using this function we are able to stamp milliseconds (because we make a system call to kernel32.dll
   FileWrite(fileHandle, timestamp, Bid, Ask);
   // Print("Written to ", fileName, ": ", timestamp, " ", Bid, " ", Ask);  // this is what is written to the journal tab in metatrader window
   FileFlush(fileHandle); // save the file
   return(0);
}
//---------------------------------------------------------------------------

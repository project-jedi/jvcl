{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License 
Version 1.1 (the "License"); you may not use this file except in compliance 
with the License. You may obtain a copy of the License at 
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, 
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for 
the specific language governing rights and limitations under the License. 

The Original Code is: JvAVICapture.PAS, released 2003-07-05.

The Initial Developer of the Original Code is Olivier Sannier <obones@meloo.com>
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): none to date


Last Modified: 2003-06-28;
Current Version: 0.2

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description: This unit defines a component that you can drop on any form or
             frame and that will display the video stream captured by a video
             device installed under Windows. You can perform live previews,
             record movies (and save them to avi files) or even capture
             single frames. A direct access is provided to the frames so that
             you can process them if you want. This is an encapsulation of the
             AVICap API from Win32.

Known Issues: none known
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvAVICapture;

interface

// Rapid boolean evaluation
{$B-}

uses Classes, Controls, Windows, VFW, MMSystem, SysUtils, Graphics, Messages;

resourcestring NOT_CONNECTED = 'Not connected';
               ERROR_MSG = 'Error #';

type
  // The video format used by the video device
  TJvVideoFormat = class (TPersistent)
  protected
    FHwnd        : HWND;            // the AVICap window using this format
    FWidth       : Cardinal;        // width of the image
    FHeight      : Cardinal;        // height of the image
    FBitDepth    : Cardinal;        // bits per pixel (8-16-24-32...)
    FPixelFormat : TPixelFormat;    // pixel format (RGB, BGR, YUV...)
    FCompression : Integer;         // compression used
    
  public
    property Width       : Cardinal     read FWidth;
    property Height      : Cardinal     read FHeight;
    property BitDepth    : Cardinal     read FBitDepth;
    property PixelFormat : TPixelFormat read FPixelFormat;
    property Compression : Integer      read FCompression;

    constructor Create;            // Create the video format
    procedure   Update;            // Update from the AVICap window
  end;

  // The audio format used by the device
  TJvAudioFormat = class (TPersistent)
  protected
    FHwnd : HWND;                // the AVICap window using this format
    FFormatTag      : Cardinal;  // the format tag (PCM or others...)
    FChannels       : Cardinal;  // number of channels (usually 1 or 2)
    FSamplesPerSec  : Cardinal;  // number of samples per second in the stream
    FAvgBytesPerSec : Cardinal;  // the average number of bytes per second
    FBlockAlign     : Cardinal;  // size of the block to align on
    FBitsPerSample  : Cardinal;  // number of bits per sample
    FExtraSize      : Cardinal;  // size of the extra data
    FExtra          : Pointer;   // extra data for formats other than PCM

  public
    // creates the audio format object and initializes it
    constructor Create;

    // updates from the AVICap window
    procedure Update;

    // apply the format to the window, returns true if successfull
    function Apply : boolean;

    // fill in a PWAVEFORMATEX structure to use with API calls
    procedure FillWaveFormatEx(var wfex : PWAVEFORMATEX);

    // run-time only property, see FSize
    property ExtraSize : Cardinal read FExtraSize write FExtraSize;
    // run-time only property, see FExtra
    property Extra : Pointer read FExtra write FExtra;

  published
    // see the relevant fields for details on the following properties
    property FormatTag      : Cardinal read FFormatTag      write FFormatTag;
    property Channels       : Cardinal read FChannels       write FChannels;
    property SamplesPerSec  : Cardinal read FSamplesPerSec  write FSamplesPerSec;
    property AvgBytesPerSec : Cardinal read FAvgBytesPerSec write FAvgBytesPerSec;
    property BlockAlign     : Cardinal read FBlockAlign     write FBlockAlign;
    property BitsPerSample  : Cardinal read FBitsPerSample  write FBitsPerSample;
  end;

  // a percentage
  TJvPercent = 0..100;

  // the number of audio buffers to use (maximum 10)
  TJvNumAudioBuffer = 0..10;

  // the type of a virtual key
  TJvVirtualKey = type Integer;

  // the capture settings to use to save a video stream to an AVI file
  TJvCaptureSettings = class (TPersistent)
  protected
    // the AVICap window that will use these settings and from which
    // we will get the values when we update them
    FHwnd : HWND;
    
    // if true, the API will popup a confirmation window when starting the
    // capture session allowing the user to choose to continue or not.
    FConfirmCapture           : Boolean;

    // the delay in microsecond between two frames. This is a requested
    // value, it may not be fully respected by the driver when capturing
    FFrameDelay: Cardinal;

    // the percentage of frames dropped above which the capture will end
    // in an error state (too many drops having occured)
    FPercentDropForError      : TJvPercent;

    // if true the capture session will be launched in a separate background
    // thread, not disabling the caller. Reentrance issues must then be
    // considered to avoid the user to launch twice the capture, for instance
    FYield                    : Boolean;

    // the requested number of video buffers. The actual number of allocated
    // buffers may well be smaller because of hardware limitations
    FNumVideoBuffer           : Cardinal;

    // the requested number of audio buffers. The actual number of allocated
    // buffers may well be smaller because of hardware limitations
    FNumAudioBuffer           : TJvNumAudioBuffer;

    // if true, the audio stream will also be captured
    FCaptureAudio             : Boolean;

    // if true, a left mouse click will stop the capture session
    FAbortLeftMouse           : Boolean;

    // if true, a right mouse click will stop the capture session
    FAbortRightMouse          : Boolean;

    // if different from 0, a press on that virtual key will stop the
    // capture session
    FKeyAbort                 : TJvVirtualKey;

    // if true, the FTimeLimit parameter will be considered
    FLimitEnabled             : Boolean;

    // the time limit for the capture session (in seconds). Will only be
    // considered if FLimitEnabled is true
    FTimeLimit                : Cardinal;

    // if true, the capture will occur at twice the size specified in the
    // other parameters of this class.
    FStepCapture2x            : Boolean;

    // the number of frames to sample and make the average of when using
    // a step capture
    FStepCaptureAverageFrames : Cardinal;

    // the size of an audio buffer
    FAudioBufferSize          : Cardinal;

    // if true, the audio stream is the master one with respect to time
    // alignment. if false, the video stream is the master (recommanded)
    FAudioMaster              : Boolean;

    // if true, the capture will controll a MCI device as its source
    FMCIControl               : Boolean;

    // if true, the step capture is enabled on the MCI device
    // this is only considered if FMCIControl is true
    FMCIStep                  : Boolean;

    // time of the MCI device to start capture at
    // this is only considered if FMCIControl is true
    FMCIStartTime             : Cardinal;

    // time of the MCI device to stop capture at
    // this is only considered if FMCIControl is true
    FMCIStopTime              : Cardinal;

    // sets the FKeyAbort field
    procedure SetKeyAbort(nKeyAbort : TJvVirtualKey);

    // get and set the FPS property
    function GetFPS: Double;
    procedure SetFPS(const Value: Double);

    // set the FrameDelay property, ensuring the value is always
    // greater than 0
    procedure SetFrameDelay(const Value: Cardinal);
  public
    // creates and initializes the class
    constructor Create;

    // updates the class fields from the AVICap window
    procedure   Update;

    // applies the class fields to the AVICap window, returns true if successful
    function    Apply : Boolean;

  published
    // please refer to the relevant field declarations for detail on the following properties
    property ConfirmCapture           : Boolean           read FConfirmCapture           write FConfirmCapture;
    property FrameDelay               : Cardinal          read FFrameDelay               write SetFrameDelay;
    property FPS                      : Double            read GetFPS                    write SetFPS;
    property PercentDropForError      : TJvPercent        read FPercentDropForError      write FPercentDropForError;
    property Yield                    : Boolean           read FYield                    write FYield;
    property NumVideoBuffer           : Cardinal          read FNumVideoBuffer           write FNumVideoBuffer;
    property NumAudioBuffer           : TJvNumAudioBuffer read FNumAudioBuffer           write FNumAudioBuffer;
    property CaptureAudio             : Boolean           read FCaptureAudio             write FCaptureAudio;
    property AbortLeftMouse           : Boolean           read FAbortLeftMouse           write FAbortLeftMouse;
    property AbortRightMouse          : Boolean           read FAbortRightMouse          write FAbortRightMouse;
    property KeyAbort                 : TJvVirtualKey     read FKeyAbort                 write SetKeyAbort;
    property LimitEnabled             : Boolean           read FLimitEnabled             write FLimitEnabled;
    property TimeLimit                : Cardinal          read FTimeLimit                write FTimeLimit;
    property StepCapture2x            : Boolean           read FStepCapture2x            write FStepCapture2x;
    property StepCaptureAverageFrames : Cardinal          read FStepCaptureAverageFrames write FStepCaptureAverageFrames;
    property AudioBufferSize          : Cardinal          read FAudioBufferSize          write FAudioBufferSize;
    property AudioMaster              : Boolean           read FAudioMaster              write FAudioMaster;
    property MCIControl               : Boolean           read FMCIControl               write FMCIControl;
    property MCIStep                  : Boolean           read FMCIStep                  write FMCIStep;
    property MCIStartTime             : Cardinal          read FMCIStartTime             write FMCIStartTime;
    property MCIStopTime              : Cardinal          read FMCIStopTime              write FMCIStopTime;
  end;

  // the type for the number of colors a palette can have
  TJvPaletteNbColors = 0..256;

  TJvPalette = class (TPersistent)
  private
    FHwnd : HWND;     // the AVICap window that will use these settings
  public
    // create the object
    constructor Create;

    // save the palette associated with the driver into the given file
    // and returns true upon success.
    function Save(filename : string) : Boolean;

    // loads the palette from the given file and returns true upon success
    // FHwnd must not be null
    function Load(filename : string) : Boolean;

    // paste the palette from the clipboard
    function PasteFromClipboard : Boolean;

    // automatically create the best palette from the first nbFrames frames with
    // a maximum of nbColors colors
    function AutoCreate(nbFrames : Integer; nbColors : TJvPaletteNbColors) : Boolean;

    // Use this call from a frame callback and set the flag to true to indicate that
    // the current frame must be considered when creating the palette. Continue
    // calling this method with flag set to true as long as you need it.
    // Then call it again with flag set to false, to finalize the palette and pass
    // it to the capture driver that will now use it.
    function ManuallyCreate(flag : Boolean; nbColors : TJvPaletteNbColors) : Boolean;
  end;

  // the driver index (-1 if not connected, 0-9 if connected as there are at most 10 drivers
  // according to Microsoft documentation. But there can be more than 1 source per driver...
  TJvDriverIndex = -1..9;

  // what a driver can do on the system
  TJvDriverCaps = set of (dcOverlay,             // overlay rendering
                          dcDlgVideoSource,      // display a dialog to choose video source
                          dcDlgVideoFormat,      // display a dialog to choose video format
                          dcDlgVideoDisplay,     // display a dialog to choose video display
                          dcCaptureInitialized,  // is the capture initialized
                          dcSuppliesPalettes);   // if the driver supplies palettes

  TJvUsedEvents = set of (ueCapControl,     // the OnCapControl event will be triggered
                          ueError,          // the OnError event will be triggered
                          ueFrame,          // the OnFrame event will be triggered
                          ueStatus,         // the OnStatus event will be triggered
                          ueVideoStream,    // the OnVideoStream event will be triggered
                          ueWaveStream,     // the OnWaveStream event will be triggered
                          ueYield);         // the OnYield event will be triggered

  // the video dialog to display
  TJvVideoDialog = (vdSource,         // the source dialog (only if dcDlgVideoSource is in the caps)
                    vdFormat,         // the format dialog (only if dcDlgVideoFormat is in the caps)
                    vdDisplay,        // the display dialog (only if dcDlgVideoDisplay is in the caps)
                    vdCompression);   // the compression dialog (with all the installed video codecs)

  // local type for the events
  TJvVideoHdr = PVIDEOHDR;
  TJvWaveHdr = PWAVEHDR;

  // forward declaration for the events
  TJvAVICapture = class;

  // the event triggered in case of an error
  // Sender is the TJvAVICapture component triggering the event
  // nErr is the error number
  // str is the string associated with that error
  TOnError = procedure (Sender : TJvAVICapture; nErr : Integer; str : string) of object;

  // the event triggered in case of a status change (use it to follow progress)
  // Sender is the TJvAVICapture component triggering the event
  // nId is the id of the status change (see win32 API for more details)
  // str is the string associated with that status change
  TOnStatus = procedure (Sender : TJvAVICapture; nId : Integer; str : string) of object;

  // the event triggerred when the driver is yielding. a good place to put a
  // call to Application.ProcessMessages
  // Sender is the TJvAVICapture component triggering the event
  TOnYield = procedure (Sender : TJvAVICapture) of object;

  // the event trigerred when a frame is ready to be written to disk during streaming capture
  // Sender is the TJvAVICapture component triggering the event
  // videoHdr is the video header describing the stream
  TOnVideoStream = procedure (Sender : TJvAVICapture; videoHdr : TJvVideoHdr) of object;

  // the event trigerred when a frame is ready, in a non streaming capture session
  TOnFrame = TOnVideoStream;

  // the event trigerred when an audio buffer is ready to be written do disk during streaming capture
  // Sender is the TJvAVICapture component triggering the event
  // audioHdr is the audio header describing the stream
  TOnWaveStream = procedure (Sender : TJvAVICapture; waveHdr : TJvWaveHdr) of object;

  // the event triggered when you want to use precise capture control
  // Sender is the TJvAVICapture component triggering the event
  // state is the state in which the capture is (refer to API for details)
  // Result is to be set to true if capture must continue, false if it must stop
  TOnCapControl = procedure (Sender : TJvAVICapture; nState : Integer; var Result : Boolean) of object;


  // the main component. Just drop it on a form or a frame, set the driver property, set previewing to
  // true and you should see the video coming through (even in design mode !)
  TJvAVICapture = class (TWinControl)
  protected
    FCaptureSettings      : TJvCaptureSettings;  // the capture settings
    FCapturing            : Boolean;             // true if capture is happening
    FConnected            : Boolean;             // true if connected to a driver
    FDrivers              : TStringList;         // the available drivers as a TStringList
    FDriverCaps           : TJvDriverCaps;       // the current driver capabilities
    FHwnd                 : HWND;                // the handle to the AviCap window
    FNoFile               : Boolean;             // true if not capturing to a file
    FOverlaying           : Boolean;             // true if using overlay display
    FPreviewFrameDelay    : Cardinal;            // the time between two preview frames (ms)
    FPreviewing           : Boolean;             // true if previewing
    FSingleFrameCapturing : Boolean;             // true if capturing using single frame capture
    FTitle                : string;              // the title of the AVICap window
    FVideoLeft            : Integer;             // the left coordinate of the displayed video
    FVideoTop             : Integer;             // the top coordinate of the displayed video

    // the user supplied event handlers
    // see respective types for details
    FOnError       : TOnError;
    FOnStatus      : TOnStatus;
    FOnYield       : TOnYield;
    FOnFrame       : TOnFrame;
    FOnVideoStream : TOnVideoStream;
    FOnWaveStream  : TOnWaveStream;
    FOnCapControl  : TOnCapControl;

    FFileName      : string;          // the filename for the capture file
    FFileSizeAlloc : Cardinal;        // the size to allocate for the capture file
    FUsedEvents    : TJvUsedEvents;   // which events are used
    FCaptureStatus : TCAPSTATUS;      // the state of the current capture
    FVideoFormat   : TJvVideoFormat;  // the current video format used (or to be used)
    FAudioFormat   : TJvAudioFormat;  // the current audio format used (or to be used)
    FScrollPos     : TPoint;          // the scrolling position in the window
    FPalette       : TJvPalette;      // the palette in use
    FDriverIndex   : TJvDriverIndex;  // the driver index (-1 if not connected)

    // the Pointer to the previous WndProc of the AviCap window
    FPreviousWndProc : Pointer;

    // window creation stuff, where the AviCap window is created:
    // what is done is that the component inherits from TWinControl and as such
    // has its own handle. We then create the AviCap window and set it as a child
    // of the TWinControl. This allows to take advantage of all the VCL handling
    // for design time, parent, ownership... and we can focus on using the
    // AviCap window to do the capture
    procedure CreateWindowHandle(const Params : TCreateParams); override;

    // destroys the AviCap window just before letting the VCL destroy the handle
    // for the TWinControl
    procedure DestroyWindowHandle; override;

    // We enforce the size of the window to be equal to the
    // video frame in this method as it is the place where it
    // should be done, rather than doing it in SetBounds
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;

    // sets the title of the AviCap window
    procedure SetTitle(nTitle : string);

    // sets the preview frame delay (the time between two frames)
    procedure SetPreviewFrameDelay(nPreviewFrameDelay : Cardinal);

    // sets and gets the preview frame rate in frames per second
    procedure SetPreviewFPS(nPreviewFPS : Double);
    function  GetPreviewFPS : Double;

    // sets the previewing property and starts or stop previewing accordingly
    procedure SetPreviewing(nPreviewing : Boolean);

    // sets and gets the filename for capture
    procedure SetFileName(nFileName : string);
    function  GetFileName : string;

    // sets the file size to allocate before capture. This might speed up capture as
    // the file won't need to be grown
    procedure SetFileSizeAlloc(nFileSizeAlloc : Cardinal);

    // sets the used events and updates the related values in the AviCap window
    procedure SetUsedEvents(nUsedEvents : TJvUsedEvents);

    // sets the overlaying rendering. May do nothing if driver cannot do overlay rendering
    procedure SetOverlaying(nOverlaying : Boolean);

    // returns the name of the driver or an empty string if FConnected is false
    function  GetDriverName : string;

    // returns the version of the driver or an empty string if FConnected is false
    function  GetDriverVersion : string;

    // set the scrolling position in the AviCap window. Useful if the frame is larger than
    // the actual size of the control
    procedure SetScrollPos(nScrollPos : TPoint);

    // sets and gets the MCI device used with this AviCap component (may well be empty)
    procedure SetMCIDevice(nMCIDevice : string);
    function  GetMCIDevice : string;

    // sets the driver index to the given value and tries to connect. If connection
    // is not possible, will not change the current value
    procedure SetDriverIndex(nIndex : TJvDriverIndex);

    // tries to starts or stops capture according to the value
    // immediately check the value of FCapturing to see if capture
    // started succesfuly
    procedure SetCapturing(nCapturing : Boolean);

    // tries starts or stops single frame capture according to the value
    // immediately check the value of FSingleFrameCapturing to see
    // if capture started succesfuly
    procedure SetSingleFrameCapturing(const Value: Boolean);

    // sets the FNoFile flag
    procedure SetNoFile(nNoFile : Boolean);

    // sets the FVideoLeft and FVideoTop values and also
    // makes the required capCall
    procedure SetVideoLeft(const Value: Integer);
    procedure SetVideoTop(const Value: Integer);

    // updates the content of the FDriverCaps field
    procedure UpdateCaps;

    // updates the content of the FCaptureStatus field
    procedure UpdateCaptureStatus;

    // stops and start using callbacks. This is required as it appears that the
    // callbacks are still called after a capture session has been stopped.
    procedure StopCallbacks;
    procedure RestartCallbacks;

    // Functions to be called from the callbacks that will trigger the user events
    procedure DoError(errId : Integer; str : string);
    procedure DoStatus(nID : Integer; str : string);
    procedure DoYield;
    procedure DoFrame(videoHdr : PVIDEOHDR);
    procedure DoVideoStream(videoHdr : PVIDEOHDR);
    procedure DoWaveStream(waveHdr : PWAVEHDR);
    procedure DoCapControl(nState : Integer; var result : Boolean);
  public
    // creates the component and initializes the different fields
    constructor Create(AOwner : TComponent); override;

    // destroys the component
    destructor Destroy; override;

    // sets the size of the component
    procedure SetBounds(nLeft, nTop, nWidth, nHeight : Integer); override;

    // enumarate the drivers and populates the FDrivers list
    procedure EnumDrivers;

    // tries to connect to the given driver. Returns true if successful, false otherwise
    function Connect(driver : TJvDriverIndex) : Boolean;

    // tries to disconnect from a driver. Returns true if successful, false otherwise
    function Disconnect : Boolean;

    // shows the given dialog and returns true if user pressed ok. If the driver
    // cannot show the given dialog...
    function ShowDialog(dialog : TJvVideoDialog) : Boolean;

    // starts and stop previewing, returning true upon success
    function StartPreview : Boolean;
    function StopPreview : Boolean;

    // start capturing to a file using streaming capture
    function StartCapture : Boolean;

    // start capturing without using a file. You should use the OnVideoStream event in that
    // case to process the frames yourself. This might be useful in a videoconferencing
    // software, where you transfer the frames directly
    function StartCaptureNoFile : Boolean;

    // stops the capture properly
    function StopCapture : Boolean;

    // aborts the capture, leaving the file unusable
    function AbortCapture : Boolean;

    // starts frame by frame capture (non streaming)
    function StartSingleFrameCapture : Boolean;

    // captures one frame in a frame by frame capture session
    function CaptureFrame : Boolean;

    // stops frame by frame capture
    function StopSingleFrameCapture : Boolean;

    // starts and stop overlay rendering, returns true if successful
    function StartOverlay : Boolean;
    function StopOverlay : Boolean;

    // applies the capture settings, returns true if successful
    function ApplyCaptureSettings : Boolean;

    // applies the audio format settings, returns true if successful
    function ApplyAudioFormat : Boolean;

    // saves the stream under the given filename
    function SaveAs(name : string) : Boolean;

    // sets information chunks in the output file
    function SetInfoChunk(chunk : TCAPINFOCHUNK) : Boolean;

    // saves the latest captured frame to a DIB file
    function SaveDIB(name : string) : Boolean;

    // copies the latest frame to the clipboard
    function CopyToClipboard : Boolean;

    // grabs one frame, not using any capture session
    // if stop is true, previewing and overlaying are stopped
    // if stop is false, previewing and overlaying are left untouched
    function GrabFrame(stop : Boolean) : Boolean;

    // public properties (run-time only), refer to fields and methods descriptions
    // for details on the usage
    property CaptureStatus : TCAPSTATUS            read FCaptureStatus;
    property Capturing     : Boolean               read FCapturing            write SetCapturing;
    property Connected     : Boolean               read FConnected;
    property DriverCaps    : TJvDriverCaps         read FDriverCaps;
    property DriverName    : string                read GetDriverName;
    property DriverVersion : string                read GetDriverVersion;
    property Drivers       : TStringList           read FDrivers;
    property Hwnd          : HWND                  read FHwnd;
    property Palette       : TJvPalette            read FPalette;
    property SingleFrameCapturing : Boolean        read FSingleFrameCapturing write SetSingleFrameCapturing;
    property VideoFormat          : TJvVideoFormat read FVideoFormat;

  published
    // published properties, refer to the field and methods descriptions for details
    property AudioFormat       : TJvAudioFormat     read FAudioFormat;
    property CaptureSettings   : TJvCaptureSettings read FCaptureSettings;
    property DriverIndex       : TJvDriverIndex     read FDriverIndex       write SetDriverIndex;
    property FileName          : string             read GetFileName        write SetFileName;
    property FileSizeAlloc     : Cardinal           read FFileSizeAlloc     write SetFileSizeAlloc;
    property MCIDevice         : string             read GetMCIDevice       write SetMCIDevice;
    property NoFile            : Boolean            read FNoFile            write SetNoFile;
    property Overlaying        : Boolean            read FOverlaying        write SetOverlaying;
    property PreviewFrameDelay : Cardinal           read FPreviewFrameDelay write SetPreviewFrameDelay;
    property PreviewFPS        : Double             read GetPreviewFPS      write SetPreviewFPS;
    property Previewing        : Boolean            read FPreviewing        write SetPreviewing;
    property ScrollPos         : TPoint             read FScrollPos         write SetScrollPos;
    property Title             : string             read FTitle             write SetTitle;
    property UsedEvents        : TJvUsedEvents      read FUsedEvents        write SetUsedEvents;
    property VideoLeft         : Integer            read FVideoLeft         write SetVideoLeft;
    property VideoTop          : Integer            read FVideoTop          write SetVideoTop;

    // inherited properties getting published
    property AutoSize;
    property ParentShowHint;
    property ShowHint;
    property Visible;

    // the events, refer to the fields decriptions for details
    property OnError       : TOnError       read FOnError       write FOnError;
    property OnStatus      : TOnStatus      read FOnStatus      write FOnStatus;
    property OnYield       : TOnYield       read FOnYield       write FOnYield;
    property OnFrame       : TOnFrame       read FOnFrame       write FOnFrame;
    property OnVideoStream : TOnVideoStream read FOnVideoStream write FOnVideoStream;
    property OnWaveStream  : TOnWaveStream  read FOnWaveStream  write FOnWaveStream;
    property OnCapControl  : TOnCapControl  read FOnCapControl  write FOnCapControl;
  end;

implementation

uses Math;  // for min and max

const
  // minimal height and width of the display window
  CMinHeight : Integer = 20;
  CMinWidth  : Integer = 20;

{ Global functions }

// an helper function that tells if the window is connected to a driver
function capDriverConnected(hwnd : HWND) : boolean;
var tmpName : array [0..MAX_PATH] of char;
begin
  Result := capDriverGetName(hwnd, tmpName, sizeof(tmpName));
end;

{ This is the custom window procedure, which replaces the one originally associated
  with the AviCap window. all we do is pass the messages to the TWinControl
  containing the AviCap window so that it can resize and move itself.
  Then we pass the message to the original window procedure for it to handle the
  messages it needs to perform the video capture
}
function CustomWndProc(hwnd : HWND; msg : UINT; wParam : WPARAM; lParam : LPARAM): LRESULT; stdcall;
var Self : TJvAVICapture;
begin
  Result := 0;

  // get the Pointer to self from the window user data
  Self := TJvAVICapture(GetWindowLong(hwnd, GWL_USERDATA));
  if self <> nil then
  begin
    // send the message to the containing window,
    // except for WM_NCHITTEST during design
    // This will prevent 100% processor usage when the mouse is kept over
    // the control during design time
    if not ((msg = WM_NCHITTEST) and (csDesigning in self.ComponentState)) then 
    begin
      PostMessage(self.Handle, msg, wParam, lParam);
    end;

    // sending the message to the original window proc
    Result := CallWindowProc(self.FPreviousWndProc, hwnd, msg, wParam, lParam);
  end;
end;


{ Callbacks }

// This is the callback called in case of an error
// will only be called if the user chose so with ueError
function ErrorCallback(hwnd : HWND; errID : Integer; str : LPSTR) : LRESULT; stdcall;
var self : TJvAVICapture;
begin
  // clear previous error if required
  if errID = 0 then
  begin
    Result := LRESULT(TRUE);
    exit;
  end;

  // get the Pointer to self from the window user data
  Self := TJvAVICapture(GetWindowLong(hwnd, GWL_USERDATA));
  if self <> nil then
  begin
    self.DoError(errId, str);
  end;

  Result := LRESULT(TRUE);
end;

// This is the callback called in case of a status change
// will only be called if the user chose so with ueStatus
function StatusCallback(hwnd : HWND; nID : Integer; str : LPSTR) : LRESULT; stdcall;
var Self : TJvAVICapture;
begin
  // get the Pointer to self from the window user data
  Self := TJvAVICapture(GetWindowLong(hwnd, GWL_USERDATA));
  if self <> nil then
  begin
    self.DoStatus(nId, str);
  end;

  result := LRESULT(TRUE);
end;

// This is the callback called in case of yielding
// will only be called if the user chose so with ueYield
function YieldCallback(hwnd : HWND) : LRESULT; stdcall;
var Self : TJvAVICapture;
begin
  // get the Pointer to self from the window user data
  Self := TJvAVICapture(GetWindowLong(hwnd, GWL_USERDATA));
  if self <> nil then 
  begin
    self.DoYield;
  end;

  result := LRESULT(TRUE);
end;

// This is the callback called in case a new frame is available while a non
// streaming capture is in progress
// will only be called if the user chose so with ueFrame
function FrameCallback(hwnd : HWND; videoHdr : PVIDEOHDR) : LRESULT; stdcall;
var Self : TJvAVICapture;
begin
  // get the Pointer to self from the window user data
  Self := TJvAVICapture(GetWindowLong(hwnd, GWL_USERDATA));
  if self <> nil then
  begin
    self.DoFrame(videoHdr);
  end;

  Result := LRESULT(TRUE);
end;

// This is the callback called when a frame is available, just before being
// written to disk, only if using stream capture
// will only be called if the user chose so with ueVideoStream
function VideoStreamCallback(hwnd : HWND; videoHdr : PVIDEOHDR) : LRESULT; stdcall;
var self : TJvAVICapture;
begin
  // get the Pointer to self from the window user data
  Self := TJvAVICapture(GetWindowLong(hwnd, GWL_USERDATA));
  if self <> nil then 
  begin
    self.DoVideoStream(videoHdr);
  end;

  Result := LRESULT(TRUE);
end;

// this is the callback when an audio buffer is ready to be written to disk
// and only when using streaming capture
// will only be called if user chose so withe ueWaveStream
function WaveStreamCallback(hwnd : HWND; waveHdr : PWAVEHDR) : LRESULT; stdcall;
var self : TJvAVICapture;
begin
  // get the Pointer to self from the window user data
  Self := TJvAVICapture(GetWindowLong(hwnd, GWL_USERDATA));
  if self <> nil then
  begin
    self.DoWaveStream(waveHdr);
  end;

  Result := LRESULT(TRUE);
end;

// this is the callback called when a precise capture control event has
// occured. Only called if user chose so with ueCapControl
function CapControlCallback(hwnd : HWND; nState : Integer) : LRESULT; stdcall;
var self : TJvAVICapture;
    res : Boolean;
begin
  res := true;
  // get the Pointer to self from the window user data
  Self := TJvAVICapture(GetWindowLong(hwnd, GWL_USERDATA));
  if self <> nil then
  begin
    self.DoCapControl(nState, res);
  end;

  Result := LRESULT(res);
end;

{ TJvVideoFormat }

constructor TJvVideoFormat.Create;
begin
  inherited Create;

  FHwnd := 0;
end;

procedure TJvVideoFormat.Update;
var bmpInfo : BITMAPINFOHEADER;
begin
  if (fHWnd <> 0) and capDriverConnected(FHwnd) then
  begin
    // get format from the AviCap window
    capGetVideoFormat(FHwnd, @bmpInfo, sizeof(bmpInfo));

    // update the internal values
    FWidth       := bmpInfo.biWidth;
    FHeight      := bmpInfo.biHeight;
    FBitDepth    := bmpInfo.biBitCount;
    FCompression := bmpInfo.biCompression;

    case BitDepth of
      0  : FPixelFormat := pfDevice;
      1  : FPixelFormat := pf1bit;
      4  : FPixelFormat := pf4bit;
      8  : FPixelFormat := pf8bit;
      16 : FPixelFormat := pf15bit;
      24 : FPixelFormat := pf24bit;
      32 : FPixelFormat := pf32bit;
      else FPixelFormat := pfCustom;
    end;
  end;
end;


{ TJvAudioFormat }

constructor TJvAudioFormat.Create;
begin
  Inherited Create;

  FHwnd := 0;
  FExtra := nil;
end;

procedure TJvAudioFormat.Update;
var info : TWAVEFORMATEX;
begin
  if (FHwnd <> 0) and capDriverConnected(FHwnd) then
  begin
    // gets the format from the AviCap window
    capGetAudioFormat(FHwnd, @info, sizeof(info));

    // sets the internal values
    FFormatTag      := info.wFormatTag;
    FChannels       := info.nChannels;
    FSamplesPerSec  := info.nSamplesPerSec;
    FAvgBytesPerSec := info.nAvgBytesPerSec;
    FBlockAlign     := info.nBlockAlign;
    FBitsPerSample  := info.wBitsPerSample;
    FExtraSize      := info.cbSize;

    // if there is extra data, save it too
    if FExtraSize > 0 then
    begin
      // if there was extra data saved before, free it before
      if FExtra <> nil then
      begin
        FreeMem(FExtra);
      end;
      GetMem(FExtra, ExtraSize);
      CopyMemory(FExtra, (PChar(@info))+sizeof(TWAVEFORMATEX), FExtraSize);
    end;
  end;
end;

function TJvAudioFormat.Apply : Boolean;
var pwfex : PWAVEFORMATEX;
begin
  Result := False;
  if FHwnd <> 0 then 
  begin
    FillWaveFormatEx(pwfex);
    Result := capSetAudioFormat(FHwnd, pwfex, sizeof(TWAVEFORMATEX)+pwfex^.cbSize);
  end;
end;

procedure TJvAudioFormat.FillWaveFormatEx(var wfex : PWAVEFORMATEX);
begin
  case FormatTag of
    WAVE_FORMAT_PCM : 
      begin
        GetMem(wfex, sizeof(TWAVEFORMATEX));
        wfex^.wFormatTag := FFormatTag;
        // ensure maximum 2 channels
        wfex^.nChannels := FChannels mod 3;
        wfex^.nSamplesPerSec := FSamplesPerSec;
        // ensure 8 or 16 bits
        wfex^.wBitsPerSample := ((FBitsPerSample div 8) mod 3) * 8;
        // using rules defined in Documentation
        wfex^.nBlockAlign := wfex.nChannels * wfex.wBitsPerSample div 8;
        wfex^.nAvgBytesPerSec := wfex.nSamplesPerSec * wfex.nBlockAlign;
        wfex^.cbSize := 0;
      end;
    else 
      begin
        GetMem(wfex, sizeof(TWAVEFORMATEX)+FExtraSize);
        wfex^.wFormatTag := FFormatTag;
        wfex^.nChannels := FChannels;
        wfex^.nSamplesPerSec := FSamplesPerSec;
        wfex^.nAvgBytesPerSec := FAvgBytesPerSec;
        wfex^.nBlockAlign := FBlockAlign;
        wfex^.wBitsPerSample := FBitsPerSample;
        wfex^.cbSize := FExtraSize;

        // copy Extra to the end of the structure
        CopyMemory((PChar(@wfex))+sizeof(TWAVEFORMATEX), FExtra, FExtraSize);
     end;
  end;
end;


{ TJvCaptureSettings }

procedure TJvCaptureSettings.SetKeyAbort(nKeyAbort : TJvVirtualKey);
var Modifiers : Word;
begin
  // Unregister any previous hotkey
  if FKeyAbort <> 0 then
    UnregisterHotKey(FHwnd, 0);

  // register hotkey, only if needed
  if nKeyAbort <> 0 then
  begin
    Modifiers := 0;
    if (nKeyAbort and $4000) <> 0 then
      Modifiers := Modifiers or MOD_SHIFT;
    if (nKeyAbort and $8000) <> 0 then
      Modifiers := Modifiers or MOD_CONTROL;
    if RegisterHotkey(FHwnd, 0, Modifiers, nKeyAbort and $FF) then
    begin
      FKeyAbort := nKeyAbort;
    end;
  end
  else
    FKeyAbort := nKeyAbort;
end;

constructor TJvCaptureSettings.Create;
begin
  Inherited Create;

  FHwnd := 0;
  FFrameDelay := 1;
end;

procedure TJvCaptureSettings.Update;
var parms : TCAPTUREPARMS;
begin
  if FHwnd <> 0 then
  begin
    // get capture settings from window
    capCaptureGetSetup(FHwnd, @parms, sizeof(parms));
    
    // udapte internal settings
    with parms do
    begin
      FFrameDelay               := dwRequestMicroSecPerFrame;
//      FFramesPerSec             := 1/dwRequestMicroSecPerFrame*1E6;
      FConfirmCapture           := fMakeUserHitOKToCapture;
      FPercentDropForError      := wPercentDropForError;
      FYield                    := FYield;
      FNumVideoBuffer           := wNumVideoRequested;
      FCaptureAudio             := FCaptureAudio;
      FNumAudioBuffer           := wNumAudioRequested;
      FAbortLeftMouse           := FAbortLeftMouse;
      FAbortRightMouse          := FAbortRightMouse;
      FKeyAbort                 := vKeyAbort;
      FLimitEnabled             := FLimitEnabled;
      FTimeLimit                := wTimeLimit;
      FStepCapture2x            := fStepCaptureAt2x;
      FStepCaptureAverageFrames := wStepCaptureAverageFrames;
      FAudioBufferSize          := dwAudioBufferSize;
      FAudioMaster              := (AVStreamMaster = AVSTREAMMASTER_AUDIO);
      FMCIControl               := FMCIControl;
      FMCIStep                  := fStepMCIDevice;
      FMCIStartTime             := dwMCIStartTime;
      FMCIStopTime              := dwMCIStopTime;
    end;
  end;
end;

function TJvCaptureSettings.Apply : Boolean;
var parms : TCAPTUREPARMS;
begin
  Result := false;
  if FHwnd <> 0 then
  begin
    // get original values from window
    capCaptureGetSetup(FHwnd, @parms, sizeof(parms));
    
    // set our own values
    with parms do
    begin
      dwRequestMicroSecPerFrame := FFrameDelay;
      fMakeUserHitOKToCapture   := ConfirmCapture;
      wPercentDropForError      := PercentDropForError;
      FYield                    := Yield;
      wNumVideoRequested        := NumVideoBuffer;
      FCaptureAudio             := CaptureAudio;
      wNumAudioRequested        := NumAudioBuffer;
      FAbortLeftMouse           := AbortLeftMouse;
      FAbortRightMouse          := AbortRightMouse;
      vKeyAbort                 := FKeyAbort;
      FLimitEnabled             := LimitEnabled;
      wTimeLimit                := TimeLimit;
      fStepCaptureAt2x          := StepCapture2x;
      wStepCaptureAverageFrames := StepCaptureAverageFrames;
      dwAudioBufferSize         := AudioBufferSize;
      if AudioMaster then
      begin
        AVStreamMaster := AVSTREAMMASTER_AUDIO;
      end
      else
      begin
        AVStreamMaster := AVSTREAMMASTER_NONE;
      end;
      FMCIControl    := self.FMCIControl;
      fStepMCIDevice := self.FMCIStep;
      dwMCIStartTime := FMCIStartTime;
      dwMCIStopTime  := FMCIStopTime;
    end;

    // apply new settings
    Result := capCaptureSetSetup(FHwnd, @parms, sizeof(parms));
  end;
end;


function TJvCaptureSettings.GetFPS: Double;
begin
  Result := 1/FFrameDelay * 1E6;
end;

procedure TJvCaptureSettings.SetFPS(const Value: Double);
begin
  FFrameDelay := round(1E6/Value);
end;

procedure TJvCaptureSettings.SetFrameDelay(const Value: Cardinal);
begin
  // to avoid division by 0 and stupid value for a time delay
  // between two frames
  if Value = 0 then
    FFrameDelay := 1
  else
    FFrameDelay := Value;
end;

{ TJvPalette }

constructor TJvPalette.Create;
begin
  Inherited Create;

  FHwnd := 0;
end;

function TJvPalette.Save(filename : string) : Boolean;
begin
  Result := (FHwnd <> 0) and capPaletteSave(FHwnd, PChar(filename));
end;

function TJvPalette.Load(filename : string) : Boolean;
begin
  Result := (FHwnd <> 0) and capPaletteOpen(FHwnd, PChar(filename));
end;

function TJvPalette.PasteFromClipboard : Boolean;
begin
  Result := (FHwnd <> 0) and capPalettePaste(FHwnd);
end;

function TJvPalette.AutoCreate(nbFrames : Integer; nbColors : TJvPaletteNbColors) : Boolean;
begin
  Result := (FHwnd <> 0) and capPaletteAuto(FHwnd, nbFrames, nbColors);
end;

function TJvPalette.ManuallyCreate(flag : Boolean; nbColors : TJvPaletteNbColors) : Boolean;
begin
  Result := (FHwnd <> 0) and capPaletteManual(FHwnd, flag, nbColors);
end;

{ TJvAVICapture }

procedure TJvAVICapture.CreateWindowHandle(const Params : TCreateParams);
begin
  // ensure the TWinControl is fully created first
  inherited CreateWindowHandle(Params);

  // no hint to show
  //ParentShowHint := false;
  //ShowHint := false;

  // create the AviCap window
  FHwnd := capCreateCaptureWindow(PChar(Title),       // use the user defined title
                                  WS_VISIBLE or       // window is visible
                                  WS_CHILD and        // it's a child window
                                  not WS_CAPTION and  // it has no caption
                                  not WS_BORDER,      // it has no border
                                  0,                  // 0 left coordinate
                                  0,                  // 0 top coordinate
                                  320,                // width defaults to 320
                                  240,                // heights defaults to 240
                                  Handle,             // child of the TWinControl
                                  0);                 // window identifier

  // place the Pointer to self in the user data
  SetWindowLong(FHwnd, GWL_USERDATA, Integer(self));

  // replace the WndProc to be ours
  FPreviousWndProc := Pointer(GetWindowLong(FHwnd, GWL_WNDPROC));
  SetWindowLong(FHwnd, GWL_WNDPROC, Integer(@CustomWndProc));

  // updates the FHwnd member of audio format, capture settings, palette and video format
  // yes, they are private members, but they can still be accessed by a foreign class
  // because the access is done in the same pas file !
  FAudioFormat.FHwnd     := FHwnd;
  FCaptureSettings.FHwnd := FHwnd;
  FPalette.FHwnd         := FHwnd;
  FVideoFormat.FHwnd     := FHwnd;

  // sets the callbacks
  UsedEvents := fUsedEvents;
end;

procedure TJvAVICapture.DestroyWindowHandle;
begin
  // restore the window proc
  SetWindowLong(FHwnd, GWL_WNDPROC, Integer(FPreviousWndProc));

  // destroy the AviCap Window
  DestroyWindow(FHwnd);
  
  // let the TWinControl window be destroyed
  inherited;
end;

procedure TJvAVICapture.SetTitle(nTitle : string);
begin
  if FHwnd <> 0 then 
  begin
    FTitle := nTitle;
    SetWindowText(FHwnd, PChar(FTitle));
  end;
end;

procedure TJvAVICapture.SetPreviewFrameDelay(nPreviewFrameDelay : Cardinal);
begin
  FPreviewFrameDelay := nPreviewFrameDelay;
  if Previewing then
  begin
    StopPreview;
    StartPreview;
  end;
end;

procedure TJvAVICapture.SetPreviewFPS(nPreviewFPS : Double);
begin
  SetPreviewFrameDelay(round(1E3*1/nPreviewFPS));
end;

function  TJvAVICapture.GetPreviewFPS : Double;
begin
  Result := 1E3*1/FPreviewFrameDelay;
end;

procedure TJvAVICapture.SetPreviewing(nPreviewing : Boolean);
begin
  if nPreviewing = false and Previewing then 
  begin
    StopPreview;
  end;
  if nPreviewing = true and not Previewing then 
  begin
    StartPreview;
  end;
end;

procedure TJvAVICapture.SetFileName(nFileName : string);
begin
  if FHwnd <> 0 then 
  begin
    FFileName := nFileName;
    // change the filename
    capFileSetCaptureFile(FHwnd, PChar(nFileName));
  end;
end;

function TJvAVICapture.GetFileName : string;
var name : array [0..MAX_PATH] of char;
begin
  if FHwnd <> 0 then 
  begin
    // get the filename from the window
    capFileGetCaptureFile(FHwnd, name, sizeof(name));
    FFileName := name;
  end;
  Result := FFileName;
end;

procedure TJvAVICapture.SetFileSizeAlloc(nFileSizeAlloc : Cardinal);
begin
  if FHwnd <> 0 then 
  begin
    FFileSizeAlloc := nFileSizeAlloc;
    capFileAlloc(FHwnd, FFileSizeAlloc);
  end;
end;

procedure TJvAVICapture.SetUsedEvents(nUsedEvents : TJvUsedEvents);
begin
  FUsedEvents := nUsedEvents;

  if FHwnd <> 0 then 
  begin
    if ueError in FUsedEvents then
      capSetCallbackOnError(fhwnd, @ErrorCallback)
    else
      capSetCallbackOnError(fhwnd, nil);

    if ueStatus in FUsedEvents then
      capSetCallbackOnStatus(fhwnd, @StatusCallback)
    else
      capSetCallbackOnStatus(fhwnd, nil);

    if ueYield in FUsedEvents then
      capSetCallbackOnYield(fhwnd, @YieldCallback)
    else
      capSetCallbackOnYield(fhwnd, nil);

    if ueFrame in FUsedEvents then
      capSetCallbackOnFrame(fhwnd, @FrameCallback)
    else
      capSetCallbackOnFrame(fhwnd, nil);

    if ueVideoStream in FUsedEvents then
      capSetCallbackOnVideoStream(fhwnd, @VideoStreamCallback)
    else
      capSetCallbackOnVideoStream(fhwnd, nil);

    if ueWaveStream in FUsedEvents then
      capSetCallbackOnWaveStream(fhwnd, @WaveStreamCallback)
    else
      capSetCallbackOnWaveStream(fhwnd, nil);

    if ueCapControl in FUsedEvents then
      capSetCallbackOnCapControl(fhwnd, @CapControlCallback)
    else
      capSetCallbackOnCapControl(fhwnd, nil);
  end;
end;

procedure TJvAVICapture.SetOverlaying(nOverlaying : Boolean);
begin
  if not nOverlaying = false then
  begin
    if Overlaying then
      StopOverlay;
  end
  else
    if not Overlaying then
      StartOverlay;
end;

function TJvAVICapture.GetDriverName : string;
var name : array[0..MAX_PATH] of char;
begin
  if FHwnd <> 0 then 
  begin
    capDriverGetName(FHwnd, name, sizeof(name));
    Result := name;
  end
  else 
  begin
    Result := NOT_CONNECTED;
  end;
end;

function TJvAVICapture.GetDriverVersion : string;
var version : array[0..MAX_PATH] of char;
begin
  if FHwnd <> 0 then 
  begin
    capDriverGetVersion(FHwnd, version, sizeof(version));
    Result := version;
  end
  else 
  begin
    Result := NOT_CONNECTED;
  end;
end;

procedure TJvAVICapture.SetScrollPos(nScrollPos : TPoint);
begin
  if FHwnd <> 0 then 
  begin
    FScrollPos := nScrollPos;
    capSetScrollPos(FHwnd, @FScrollPos);
  end;
end;

procedure TJvAVICapture.SetMCIDevice(nMCIDevice : string);
begin
  if FHwnd <> 0 then 
  begin
    capSetMCIDeviceName(FHwnd, PChar(nMCIDevice));
  end;
end;

function TJvAVICapture.GetMCIDevice : string;
var name : array [0..MAX_PATH] of char;
begin
  if FHwnd <> 0 then 
  begin
    capGetMCIDeviceName(fhwnd, name, sizeof(name));
    Result := name;
  end
  else 
  begin
    Result := NOT_CONNECTED;
  end;
end;

procedure TJvAVICapture.SetDriverIndex(nIndex : TJvDriverIndex);
begin
  if Connect(nIndex) then 
  begin
    FDriverIndex := nIndex;
  end;
end;

procedure TJvAVICapture.SetCapturing(nCapturing : Boolean);
begin
  if FCapturing then
  begin
    if not nCapturing then
      StopCapture;
  end
  else 
    if nCapturing then
    begin
      if FNoFile then 
        StartCaptureNoFile
      else
        StartCapture;
    end;
end;

procedure TJvAVICapture.SetNoFile(nNoFile : Boolean);
begin
  // only allow to change if not capturing
  if not FCapturing then 
  begin
    FNoFile := nNoFile;
  end;
end;

procedure TJvAVICapture.UpdateCaps;
var s : TCAPDRIVERCAPS;
begin
  if FHwnd <> 0 then 
  begin
    // get value from the window
    capDriverGetCaps(FHwnd, @s, sizeof(s));

    // update internal value
    FDriverCaps := [];
    if s.fHasOverlay then 
    begin
      FDriverCaps := FDriverCaps + [dcOverlay];
    end;
    if s.fHasDlgVideoSource then 
    begin
      FDriverCaps := FDriverCaps + [dcDlgVideoSource];
    end;
    if s.fHasDlgVideoFormat then 
    begin
      FDriverCaps := FDriverCaps + [dcDlgVideoFormat];
    end;
    if s.fHasDlgVideoDisplay then 
    begin
      FDriverCaps := FDriverCaps + [dcDlgVideoDisplay];
    end;
    if s.fCaptureInitialized then 
    begin
      FDriverCaps := FDriverCaps + [dcCaptureInitialized];
    end;
    if s.fDriverSuppliesPalettes then 
    begin
      FDriverCaps := FDriverCaps + [dcSuppliesPalettes];
    end;
  end;
end;

procedure TJvAVICapture.UpdateCaptureStatus;
begin
  if FHwnd <> 0 then 
  begin
    capGetStatus(FHwnd, @FCaptureStatus, sizeof(FCaptureStatus));
    FCapturing  := FCaptureStatus.fCapturingNow;
    FPreviewing := FCaptureStatus.fLiveWindow;
    FOverlaying := FCaptureStatus.fOverlayWindow;
  end;
end;

procedure TJvAVICapture.StopCallbacks;
begin
  if FHwnd <> 0 then 
  begin
    if not (csDesigning in ComponentState) then
      capSetCallbackOnError(fhwnd, nil);

    capSetCallbackOnStatus(fhwnd, nil);
    capSetCallbackOnYield(fhwnd, nil);
    capSetCallbackOnFrame(fhwnd, nil);
    capSetCallbackOnVideoStream(fhwnd, nil);
    capSetCallbackOnWaveStream(fhwnd, nil);
    capSetCallbackOnCapControl(fhwnd, nil);
  end;
end;

procedure TJvAVICapture.RestartCallbacks;
begin
  UsedEvents := FUsedEvents;
end;

constructor TJvAVICapture.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  // Not connected yet
  FDriverIndex := -1;

  // create the string list for the drivers
  FDrivers := TStringList.Create;

  // Preview frame delay = 50ms between frames (20 frames per second)
  FPreviewFrameDelay := 50;

  // Create the video format
  FVideoFormat := TJvVideoFormat.Create;

  // Create the audio format
  FAudioFormat := TJvAudioFormat.Create;
  // Default to PCM, 11.025khz 8 bit Mono
  With FAudioFormat do 
  begin
    FormatTag     := WAVE_FORMAT_PCM;
    Channels      := 1;
    BitsPerSample := 8;
    SamplesPerSec := 11025;
  end;

  // Create the capture settings
  FCaptureSettings := TJvCaptureSettings.Create;

  // Create the palete object
  FPalette := TJvPalette.Create;

  // set bounds
  SetBounds(0, 0, 320, 240);

  // enumerate the available drivers
  EnumDrivers;

  // set all events to 'used'
  UsedEvents := [ueError, ueStatus, ueYield, ueFrame, ueVideoStream, ueWaveStream, ueCapControl];

end;

destructor TJvAVICapture.Destroy;
begin
  // disconnect
  Disconnect;

  // free the embedded objects
  FDrivers.Free;              
  FCaptureSettings.Free;
  FAudioFormat.Free;
  FVideoFormat.Free;
  FPalette.Free;

  inherited;
end;

procedure TJvAVICapture.SetBounds(nLeft, nTop, nWidth, nHeight : Integer);
var lWidth, lHeight : integer;
begin
  // reload video size
  FVideoFormat.Update;

  // else, force the width and height to stay in a constant interval :
  // not less than CMinHeight and CMinWidth
  // not more than the video size
  // Autosizing will have been enforced in the CanAutoSize procedure
  lHeight := Max(Min(nHeight, FVideoFormat.Height), CMinHeight);
  lWidth  := Max(Min(nWidth,  FVideoFormat.Width),  CMinWidth);

  inherited SetBounds(nLeft, nTop, lWidth, lHeight);
end;

procedure TJvAVICapture.EnumDrivers;
var i : Integer;
  deviceName : array [0..MAX_PATH] of char;
  deviceVersion : array [0..MAX_PATH] of char;
begin
  // no more than 10 drivers in the system (cf Win32 API)
  for i := 0 to 9 do 
  begin
    if capGetDriverDescription(i, deviceName, sizeof(deviceName), deviceVersion, sizeof(deviceVersion)) then 
    begin
      FDrivers.Add(deviceName);
    end; 
  end;
end;

function TJvAVICapture.Connect(driver : TJvDriverIndex) : Boolean;
begin
  // Request a handle, will create the AviCap internal window
  // will crash if no parent set
  HandleNeeded;

  if driver = -1 then
  begin
    // if driver is -1, then we disconnect
    Result := Disconnect;
    // force the video format to be 0, 0 and update the size of the control
    FVideoFormat.FHeight := 0;
    FVideoFormat.FWidth  := 0;
  end
  else
  begin
    // else we try to connect to that driver
    Result := capDriverConnect(FHwnd, driver);
    FConnected := Result;

    if FConnected then 
    begin
      // if connected successfully, update the property
      FDriverIndex := driver;
      UpdateCaps;
      FCaptureSettings.Update;
      FAudioFormat.Update;
      UpdateCaptureStatus;

    end;
  end;
  AdjustSize;
end;

function TJvAVICapture.Disconnect:Boolean;
begin
  Result := capDriverDisconnect(FHwnd);
  updateCaptureStatus;
  FConnected := false;
end;

function TJvAVICapture.ShowDialog(dialog : TJvVideoDialog) : Boolean;
begin
  Result := false;
  if FHwnd <> 0 then 
  begin
    case dialog of
      vdSource      : Result := capDlgVideoSource(FHwnd);
      vdFormat      : Result := capDlgVideoFormat(FHwnd);
      vdDisplay     : Result := capDlgVideoDisplay(FHwnd);
      vdCompression : Result := capDlgVideoCompression(FHwnd);
    end;
    // update everything to reflect user changes
    updateCaps;
    VideoFormat.Update;
    AudioFormat.Update;
    CaptureSettings.Update;
    setbounds(left, top, width, height);
  end;
end;

function TJvAVICapture.StartPreview : Boolean;
begin
  // if we have a valid window that is not already previewing
  if (FHwnd <> 0) and not FPreviewing then 
  begin
    capPreviewRate(FHwnd, FPreviewFrameDelay);
    FPreviewing := capPreview(FHwnd, TRUE);
    UpdateCaptureStatus;
    VideoFormat.Update;
    if FPreviewing then
    begin
      FOverlaying := false;
      RestartCallbacks;
    end;
    Result := FPreviewing;
  end
  else begin
    Result := false;
  end;
end;

function TJvAVICapture.StopPreview : Boolean;
begin
  // if we have a valid window doing previewing
  // then the result is the result of capPreview
  Result := (FHwnd <>0) and FPreviewing and capPreview(FHwnd, FALSE);

  // if succesfuly stopped preview, update internal values
  if Result then
  begin
    UpdateCaptureStatus;
    FPreviewing := false;
    StopCallbacks;
  end;
end;

function TJvAVICapture.StartCapture : Boolean;
begin
  Result := false;
  if (FHwnd <> 0) and
    not FCapturing and
    ApplyCaptureSettings and
    ApplyAudioFormat then 
  begin
    updateCaptureStatus;
    VideoFormat.Update;
    FCapturing := capCaptureSequence(FHwnd);
    if FCapturing then
    begin
      restartCallbacks;
    end;
    Result := FCapturing;
  end;
end;

function TJvAVICapture.StartCaptureNoFile : Boolean;
begin
  Result := false;
  if (FHwnd <> 0) and
    not FCapturing and
    ApplyCaptureSettings and
    ApplyAudioFormat then
  begin
    updateCaptureStatus;
    VideoFormat.Update;
    FCapturing := capCaptureSequenceNoFile(FHwnd);
    FNoFile := True;
    if FCapturing then
    begin
      restartCallbacks;
    end;
    Result := FCapturing;
  end;
end;

function TJvAVICapture.StopCapture : Boolean;
begin
  Result := (FHwnd <>0) and FCapturing and capCaptureStop(FHwnd);
  if Result then
  begin
    FCapturing := false;
    stopCallbacks;
  end;
end;

function TJvAVICapture.AbortCapture : Boolean;
begin
  Result := (FHwnd <>0) and FCapturing and capCaptureAbort(FHwnd);
  if Result then
  begin
    FCapturing := false;
    stopCallbacks;
  end;
end;

function TJvAVICapture.StartSingleFrameCapture : Boolean;
begin
  Result := (FHwnd <>0) and
            not FSingleFrameCapturing and
            capCaptureSingleFrameOpen(FHwnd);
  if Result then
  begin
    updateCaptureStatus;
    VideoFormat.Update;
    restartCallbacks;
    FSingleFrameCapturing := true;
  end;
end;

function TJvAVICapture.CaptureFrame : Boolean;
begin
  Result := (FHwnd <> 0) and
            FSingleFrameCapturing and
            capCaptureSingleFrame(FHwnd);
  updateCaptureStatus;
  VideoFormat.Update;
end;

function TJvAVICapture.StopSingleFrameCapture : Boolean;
begin
  Result := (FHwnd <> 0) and
            FSingleFrameCapturing and
            capCaptureSingleFrameClose(FHwnd);
  if Result then
  begin
    updateCaptureStatus;
    VideoFormat.Update;
    stopCallbacks;
    FSingleFrameCapturing := false;
  end;
end;

function TJvAVICapture.StartOverlay : Boolean;
begin
  Result := false;
  if (FHwnd <> 0) and not FOverlaying then 
  begin
    capPreviewRate(FHwnd, FPreviewFrameDelay);
    FOverlaying := capOverlay(FHwnd, TRUE);
    updateCaptureStatus;
    VideoFormat.Update;
    if FOverlaying then 
    begin
      FPreviewing := false;
      restartCallbacks;
    end;
    Result := FOverlaying;
  end;
end;

function TJvAVICapture.StopOverlay : Boolean;
begin
  Result := (FHwnd <> 0) and FOverlaying and capOverlay(FHwnd, FALSE);

  if Result then
  begin
    updateCaptureStatus;
    FOverlaying := false;
    stopCallbacks;
  end;
end;

function TJvAVICapture.ApplyCaptureSettings : Boolean;
begin
  Result := CaptureSettings.Apply;
end;

function TJvAVICapture.ApplyAudioFormat : Boolean;
begin
  Result := AudioFormat.Apply;
end;

function TJvAVICapture.SaveAs(name : string) : Boolean;
begin
  Result := (FHwnd <> 0) and capFileSaveAs(FHwnd, PChar(name));
end;

function TJvAVICapture.SetInfoChunk(chunk : TCAPINFOCHUNK) : Boolean;
begin
  Result := (FHwnd <> 0) and capFileSetInfoChunk(FHwnd, @chunk);
end;

function TJvAVICapture.SaveDIB(name : string) : Boolean;
begin
  Result := (FHwnd <> 0) and capFileSaveDIB(FHwnd, Pchar(name));
end;

function TJvAVICapture.CopyToClipboard : Boolean;
begin
  Result := (FHwnd <> 0) and capEditCopy(FHwnd);
end;

function TJvAVICapture.GrabFrame(stop : Boolean) : Boolean;
begin
  Result := false;
  if FHwnd <> 0 then 
  begin
    if stop then 
    begin
      FPreviewing := false;
      FOverlaying := false;
      Result := capGrabFrame(FHwnd);
    end
    else 
    begin
      Result := capGrabFrameNoStop(FHwnd);
    end;
  end;
end;

procedure TJvAVICapture.DoError(errId : Integer; str : string);
begin
  if (csDesigning in ComponentState) then 
  begin
    Windows.MessageBox(WindowHandle, PChar(str), PChar(ERROR_MSG + inttostr(errId)), MB_ICONERROR);
  end;
  if assigned(FOnError) then 
  begin
    FOnError(self, errID, str);
  end;
end;

procedure TJvAVICapture.DoStatus(nID : Integer; str : string);
begin
  updateCaptureStatus;
  if assigned(FOnStatus) then 
  begin
    FOnStatus(self, nId, str);
  end;
end;

procedure TJvAVICapture.DoYield;
begin
  updateCaptureStatus;
  if assigned(FOnYield) then 
  begin
    FOnYield(self);
  end;
end;

procedure TJvAVICapture.DoFrame(videoHdr : PVIDEOHDR);
begin
  if assigned(FOnFrame) then 
  begin
    FOnFrame(self, videoHdr);
  end;
end;

procedure TJvAVICapture.DoVideoStream(videoHdr : PVIDEOHDR);
begin
  if assigned(FOnVideoStream) then 
  begin
    FOnVideoStream(self, videoHdr);
  end;
end;

procedure TJvAVICapture.DoWaveStream(waveHdr : PWAVEHDR);
begin
  if assigned(FOnWaveStream) then 
  begin
    FOnWaveStream(self, waveHdr);
  end;
end;

procedure TJvAVICapture.DoCapControl(nState : Integer; var result : Boolean);
begin
  Result := true;
  if assigned(FOnCapControl) then
  begin
    FOnCapControl(self, nState, result);
  end;
end;

procedure TJvAVICapture.SetVideoLeft(const Value: Integer);
var p : TPoint;
begin
  p.X := Value;
  p.Y := FVideoTop;
  if capSetScrollPos(FHwnd, @p) then
    FVideoLeft := Value;
end;

procedure TJvAVICapture.SetVideoTop(const Value: Integer);
var p : TPoint;
begin
  p.X := FVideoLeft;
  p.Y := Value;
  if capSetScrollPos(FHwnd, @p) then
    FVideoTop := Value;
end;

function TJvAVICapture.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  // always possible to do autosizing
  Result := true;

  // reload video size
  FVideoFormat.Update;

  // force the width and height to be equal
  // to the one from the video (with a minimum value set
  // in case there is no video yet)
  NewHeight := Max(CMinHeight, FVideoFormat.Height);
  NewWidth  := Max(CMinWidth,  FVideoFormat.Width);
end;

procedure TJvAVICapture.SetSingleFrameCapturing(const Value: Boolean);
begin
  If Value then
    StartSingleFrameCapture
  else
    StopSingleFrameCapture;
end;

end.

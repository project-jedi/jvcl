{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAVICapture.PAS, released 2003-07-05.

The Initial Developer of the Original Code is Olivier Sannier <obones att altern dott org>
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): none to date

Current Version: 0.4

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
// $Id$

unit JvAVICapture;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Messages, VFW, MMSystem, SysUtils, Classes, Graphics, Controls,
  JvTypes;

type
  TJvScrollPos = class(TPersistent)
  protected
    FLeft: Integer;
    FTop: Integer;
  published
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
  end;

  // The video format used by the video device
  TJvVideoFormat = class(TPersistent)
  protected
    FHWnd: HWND;                // the AVICap window using this format
    FWidth: Cardinal;           // width of the image
    FHeight: Cardinal;          // height of the image
    FBitDepth: Cardinal;        // bits per pixel (8-16-24-32...)
    FPixelFormat: TPixelFormat; // pixel format (RGB, BGR, YUV...)
    FCompression: Integer;      // compression used
  public
    constructor Create; // Create the video format
    procedure Update;   // Update from the AVICap window
    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;
    property BitDepth: Cardinal read FBitDepth;
    property PixelFormat: TPixelFormat read FPixelFormat;
    property Compression: Integer read FCompression;
  end;

  // The audio format used by the device
  TJvAudioFormat = class(TPersistent)
  protected
    FHWnd: HWND;               // the AVICap window using this format
    FFormatTag: Cardinal;      // the format tag (PCM or others...)
    FChannels: Cardinal;       // number of channels (usually 1 or 2)
    FSamplesPerSec: Cardinal;  // number of samples per second in the stream
    FAvgBytesPerSec: Cardinal; // the average number of bytes per second
    FBlockAlign: Cardinal;     // size of the block to align on
    FBitsPerSample: Cardinal;  // number of bits per sample
    FExtraSize: Cardinal;      // size of the extra data
    FExtra: Pointer;           // extra data for formats other than PCM
  public
    // creates the audio format object and initializes it
    constructor Create;
    // updates from the AVICap window
    procedure Update;
    // apply the format to the window, returns True if successfull
    function Apply: Boolean;
    // fill in a PWaveFormatEx structure to use with API calls
    procedure FillWaveFormatEx(var wfex: PWaveFormatEx);
    // run-time only property, see FSize
    property ExtraSize: Cardinal read FExtraSize write FExtraSize;
    // run-time only property, see FExtra
    property Extra: Pointer read FExtra write FExtra;
  published
    // see the relevant fields for details on the following properties
    property FormatTag: Cardinal read FFormatTag write FFormatTag;
    property Channels: Cardinal read FChannels write FChannels;
    property SamplesPerSec: Cardinal read FSamplesPerSec write FSamplesPerSec;
    property AvgBytesPerSec: Cardinal read FAvgBytesPerSec write FAvgBytesPerSec;
    property BlockAlign: Cardinal read FBlockAlign write FBlockAlign;
    property BitsPerSample: Cardinal read FBitsPerSample write FBitsPerSample;
  end;

  // a percentage
  TJvPercent = 0..100;

  // the number of audio buffers to use (maximum 10)
  TJvNumAudioBuffer = 0..10;

  // the type of a virtual key
  TJvVirtualKey = type Integer;

  // the capture settings to use to save a video stream to an AVI file
  TJvCaptureSettings = class(TPersistent)
  protected
    // the AVICap window that will use these settings and from which
    // we will get the values when we update them
    FHWnd: HWND;
    // if True, the API will popup a confirmation window when starting the
    // capture session allowing the user to choose to continue or not.
    FConfirmCapture: Boolean;
    // the delay in microsecond between two frames. This is a requested
    // value, it may not be fully respected by the driver when capturing
    FFrameDelay: Cardinal;
    // the percentage of frames dropped above which the capture will end
    // in an error state (too many drops having occured)
    FPercentDropForError: TJvPercent;
    // if True the capture session will be launched in a separate background
    // thread, not disabling the caller. Reentrance issues must then be
    // considered to avoid the user to launch twice the capture, for instance
    FYield: Boolean;
    // the requested number of video buffers. The actual number of allocated
    // buffers may well be smaller because of hardware limitations
    FNumVideoBuffer: Cardinal;
    // the requested number of audio buffers. The actual number of allocated
    // buffers may well be smaller because of hardware limitations
    FNumAudioBuffer: TJvNumAudioBuffer;
    // if True, the audio stream will also be captured
    FCaptureAudio: Boolean;
    // if True, a left mouse click will stop the capture session
    FAbortLeftMouse: Boolean;
    // if True, a right mouse click will stop the capture session
    FAbortRightMouse: Boolean;
    // if different from 0, a press on that virtual key will stop the
    // capture session
    FKeyAbort: TJvVirtualKey;
    // if True, the FTimeLimit parameter will be considered
    FLimitEnabled: Boolean;
    // the time limit for the capture session (in seconds). Will only be
    // considered if FLimitEnabled is True
    FTimeLimit: Cardinal;
    // if True, the capture will occur at twice the size specified in the
    // other parameters of this class.
    FStepCapture2x: Boolean;
    // the number of frames to sample and make the average of when using
    // a step capture
    FStepCaptureAverageFrames: Cardinal;
    // the size of an audio buffer
    FAudioBufferSize: Cardinal;
    // if True, the audio stream is the master one with respect to time
    // alignment. if False, the video stream is the master (recommanded)
    FAudioMaster: Boolean;
    // if True, the capture will controll a MCI device as its source
    FMCIControl: Boolean;
    // if True, the step capture is enabled on the MCI device
    // this is only considered if FMCIControl is True
    FMCIStep: Boolean;
    // time of the MCI device to start capture at
    // this is only considered if FMCIControl is True
    FMCIStartTime: Cardinal;
    // time of the MCI device to stop capture at
    // this is only considered if FMCIControl is True
    FMCIStopTime: Cardinal;
    // sets the FKeyAbort field
    procedure SetKeyAbort(nKeyAbort: TJvVirtualKey);
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
    procedure Update;
    // applies the class fields to the AVICap window, returns True if successful
    function Apply: Boolean;
  published
    // (rom) default values would be a good idea
    // please refer to the relevant field declarations for detail on the following properties
    property ConfirmCapture: Boolean read FConfirmCapture write FConfirmCapture;
    property FrameDelay: Cardinal read FFrameDelay write SetFrameDelay;
    property FPS: Double read GetFPS write SetFPS;
    property PercentDropForError: TJvPercent read FPercentDropForError write FPercentDropForError;
    property Yield: Boolean read FYield write FYield;
    property NumVideoBuffer: Cardinal read FNumVideoBuffer write FNumVideoBuffer;
    property NumAudioBuffer: TJvNumAudioBuffer read FNumAudioBuffer write FNumAudioBuffer;
    property CaptureAudio: Boolean read FCaptureAudio write FCaptureAudio;
    property AbortLeftMouse: Boolean read FAbortLeftMouse write FAbortLeftMouse;
    property AbortRightMouse: Boolean read FAbortRightMouse write FAbortRightMouse;
    property KeyAbort: TJvVirtualKey read FKeyAbort write SetKeyAbort;
    property LimitEnabled: Boolean read FLimitEnabled write FLimitEnabled;
    property TimeLimit: Cardinal read FTimeLimit write FTimeLimit;
    property StepCapture2x: Boolean read FStepCapture2x write FStepCapture2x;
    property StepCaptureAverageFrames: Cardinal read FStepCaptureAverageFrames write FStepCaptureAverageFrames;
    property AudioBufferSize: Cardinal read FAudioBufferSize write FAudioBufferSize;
    property AudioMaster: Boolean read FAudioMaster write FAudioMaster;
    property MCIControl: Boolean read FMCIControl write FMCIControl;
    property MCIStep: Boolean read FMCIStep write FMCIStep;
    property MCIStartTime: Cardinal read FMCIStartTime write FMCIStartTime;
    property MCIStopTime: Cardinal read FMCIStopTime write FMCIStopTime;
  end;

  // the type for the number of colors a palette can have
  TJvPaletteNbColors = 0..256;

  TJvPalette = class(TPersistent)
  protected
    FHWnd: HWND; // the AVICap window that will use these settings
  public
    // create the object
    constructor Create;
    // save the palette associated with the driver into the given file
    // and returns True upon success.
    function Save(FileName: string): Boolean;
    // loads the palette from the given file and returns True upon success
    // FHWnd must not be null
    function Load(FileName: string): Boolean;
    // paste the palette from the clipboard
    function PasteFromClipboard: Boolean;
    // automatically create the best palette from the first nbFrames frames with
    // a maximum of nbColors colors
    function AutoCreate(nbFrames: Integer; nbColors: TJvPaletteNbColors): Boolean;
    // Use this call from a frame callback and set the Flag to True to indicate that
    // the current frame must be considered when creating the palette. Continue
    // calling this method with Flag set to True as long as you need it.
    // Then call it again with Flag set to False, to finalize the palette and pass
    // it to the capture driver that will now use it.
    function ManuallyCreate(Flag: Boolean; nbColors: TJvPaletteNbColors): Boolean;
  end;

  // the driver index (-1 if not connected, 0-9 if connected as there are at most 10 drivers
  // according to Microsoft documentation. But there can be more than 1 source per driver...
  TJvDriverIndex = -1..9;

  // The exception triggered when an invalid index driver index is given
  EInvalidDriverIndexError = class(EJVCLException)
  public
    constructor Create(Index: TJvDriverIndex; MaxIndex: TJvDriverIndex);
  end;

  // what a driver can do on the system
  TJvDriverCaps = set of
   (dcOverlay,            // overlay rendering
    dcDlgVideoSource,     // display a dialog to choose video source
    dcDlgVideoFormat,     // display a dialog to choose video format
    dcDlgVideoDisplay,    // display a dialog to choose video display
    dcCaptureInitialized, // is the capture initialized
    dcSuppliesPalettes);  // if the driver supplies palettes

  TJvUsedEvents = set of
   (ueCapControl,  // the OnCapControl event will be triggered
    ueError,       // the OnError event will be triggered
    ueFrame,       // the OnFrame event will be triggered
    ueStatus,      // the OnStatus event will be triggered
    ueVideoStream, // the OnVideoStream event will be triggered
    ueWaveStream,  // the OnWaveStream event will be triggered
    ueYield);      // the OnYield event will be triggered

  // the video dialog to display
  TJvVideoDialog =
   (vdSource,       // the source dialog (only if dcDlgVideoSource is in the caps)
    vdFormat,       // the format dialog (only if dcDlgVideoFormat is in the caps)
    vdDisplay,      // the display dialog (only if dcDlgVideoDisplay is in the caps)
    vdCompression); // the compression dialog (with all the installed video codecs)

  // local type for the events
  PJvVideoHdr = PVIDEOHDR;
  PJvWaveHdr = PWaveHdr;

  // forward declaration for the events
  TJvAVICapture = class;

  // the event triggered in case of an error
  // Sender is the TJvAVICapture component triggering the event
  // nErr is the error number
  // Str is the string associated with that error
  TOnError = procedure(Sender: TJvAVICapture; nErr: Integer; Str: string) of object;

  // the event triggered in case of a status change (use it to follow progress)
  // Sender is the TJvAVICapture component triggering the event
  // nId is the id of the status change (see win32 API for more details)
  // Str is the string associated with that status change
  TOnStatus = procedure(Sender: TJvAVICapture; nId: Integer; Str: string) of object;

  // the event triggerred when the driver is yielding. a good place to put a
  // call to Application.ProcessMessages
  // Sender is the TJvAVICapture component triggering the event
  TOnYield = procedure(Sender: TJvAVICapture) of object;

  // the event trigerred when a frame is ready to be written to disk during streaming capture
  // Sender is the TJvAVICapture component triggering the event
  // videoHdr is the video header describing the stream
  TOnVideoStream = procedure(Sender: TJvAVICapture; videoHdr: PJvVideoHdr) of object;

  // the event trigerred when a frame is ready, in a non streaming capture session
  TOnFrame = TOnVideoStream;

  // the event trigerred when an audio buffer is ready to be written do disk during streaming capture
  // Sender is the TJvAVICapture component triggering the event
  // audioHdr is the audio header describing the stream
  TOnWaveStream = procedure(Sender: TJvAVICapture; waveHdr: PJvWaveHdr) of object;

  // the event triggered when you want to use precise capture control
  // Sender is the TJvAVICapture component triggering the event
  // state is the state in which the capture is (refer to API for details)
  // Result is to be set to True if capture must continue, False if it must stop
  TOnCapControl = procedure(Sender: TJvAVICapture; nState: Integer; var Result: Boolean) of object;

  // the main component. Just drop it on a form or a frame, set the driver property, set previewing to
  // True and you should see the video coming through (even in design mode !)
  TJvAVICapture = class(TWinControl)
  protected
    FCaptureSettings: TJvCaptureSettings; // the capture settings
    FCapturing: Boolean;                  // True if capture is happening
    FConnected: Boolean;                  // True if connected to a driver
    FDrivers: TStringList;                // the available drivers as a TStringList
    FDriverCaps: TJvDriverCaps;           // the current driver capabilities
    FHWnd: HWND;                          // the handle to the AviCap window
    FNoFile: Boolean;                     // True if not capturing to a file
    FOverlaying: Boolean;                 // True if using overlay display
    FPreviewFrameDelay: Cardinal;         // the time between two preview frames (ms)
    FPreviewing: Boolean;                 // True if previewing
    FSingleFrameCapturing: Boolean;       // True if capturing using single frame capture
    FTitle: string;                       // the title of the AVICap window
    FVideoLeft: Integer;                  // the left coordinate of the displayed video
    FVideoTop: Integer;                   // the top coordinate of the displayed video
    // the user supplied event handlers
    // see respective types for details
    FOnError: TOnError;
    FOnStatus: TOnStatus;
    FOnYield: TOnYield;
    FOnFrame: TOnFrame;
    FOnVideoStream: TOnVideoStream;
    FOnWaveStream: TOnWaveStream;
    FOnCapControl: TOnCapControl;

    FFileName: string;            // the filename for the capture file
    FFileSizeAlloc: Cardinal;     // the size to allocate for the capture file
    FUsedEvents: TJvUsedEvents;   // which events are used
    FCaptureStatus: TCAPSTATUS;   // the state of the current capture
    FVideoFormat: TJvVideoFormat; // the current video format used (or to be used)
    FAudioFormat: TJvAudioFormat; // the current audio format used (or to be used)
    FScrollPos: TJvScrollPos;     // the scrolling position in the window
    FPalette: TJvPalette;         // the palette in use
    FDriverIndex: TJvDriverIndex; // the driver index (-1 if not connected)

    // the Pointer to the previous WndProc of the AviCap window
    FPreviousWndProc: Pointer;
    // window creation stuff, where the AviCap window is created:
    // what is done is that the component inherits from TWinControl and as such
    // has its own handle. We then create the AviCap window and set it as a child
    // of the TWinControl. This allows to take advantage of all the VCL handling
    // for design time, parent, ownership... and we can focus on using the
    // AviCap window to do the capture
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    // destroys the AviCap window just before letting the VCL destroy the handle
    // for the TWinControl
    procedure DestroyWindowHandle; override;
    // We enforce the size of the window to be equal to the
    // video frame in this method as it is the place where it
    // should be done, rather than doing it in SetBounds
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    // sets the title of the AviCap window
    procedure SetTitle(nTitle: string);
    // sets the preview frame delay (the time between two frames)
    procedure SetPreviewFrameDelay(nPreviewFrameDelay: Cardinal);
    // sets and gets the preview frame rate in frames per second
    procedure SetPreviewFPS(nPreviewFPS: Double);
    function GetPreviewFPS: Double;
    // sets the previewing property and starts or stop previewing accordingly
    procedure SetPreviewing(nPreviewing: Boolean);
    // sets and gets the filename for capture
    procedure SetFileName(nFileName: TFileName);
    function GetFileName: TFileName;
    // delivers FDrivers as TStrings for property
    function GetDrivers: TStrings;
    // sets the file size to allocate before capture. This might speed up capture as
    // the file won't need to be grown
    procedure SetFileSizeAlloc(nFileSizeAlloc: Cardinal);
    // sets the used events and updates the related values in the AviCap window
    procedure SetUsedEvents(nUsedEvents: TJvUsedEvents);
    // sets the overlaying rendering. May do nothing if driver cannot do overlay rendering
    procedure SetOverlaying(nOverlaying: Boolean);
    // returns the name of the driver or an empty string if FConnected is False
    function GetDriverName: string;
    // returns the version of the driver or an empty string if FConnected is False
    function GetDriverVersion: string;
    // set the scrolling position in the AviCap window. Useful if the frame is larger than
    // the actual size of the control
    procedure SetScrollPos(nScrollPos: TJvScrollPos);
    // sets and gets the MCI device used with this AviCap component (may well be empty)
    procedure SetMCIDevice(nMCIDevice: string);
    function GetMCIDevice: string;
    // sets the driver index to the given value and tries to connect. If connection
    // is not possible, will not change the current value
    procedure SetDriverIndex(nIndex: TJvDriverIndex);
    // tries to starts or stops capture according to the value
    // immediately check the value of FCapturing to see if capture
    // started succesfuly
    procedure SetCapturing(nCapturing: Boolean);
    // tries starts or stops single frame capture according to the value
    // immediately check the value of FSingleFrameCapturing to see
    // if capture started succesfuly
    procedure SetSingleFrameCapturing(const Value: Boolean);
    // sets the FNoFile flag
    procedure SetNoFile(nNoFile: Boolean);
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
    procedure DoError(ErrId: Integer; Str: string);
    procedure DoStatus(nId: Integer; Str: string);
    procedure DoYield;
    procedure DoFrame(videoHdr: PVIDEOHDR);
    procedure DoVideoStream(videoHdr: PVIDEOHDR);
    procedure DoWaveStream(waveHdr: PWaveHdr);
    procedure DoCapControl(nState: Integer; var AResult: Boolean);
  public
    // creates the component and initializes the different fields
    constructor Create(AOwner: TComponent); override;
    // destroys the component
    destructor Destroy; override;
    // sets the size of the component
    procedure SetBounds(nLeft, nTop, nWidth, nHeight: Integer); override;
    // enumarate the drivers and populates the FDrivers list
    procedure EnumDrivers;
    // tries to connect to the given driver. Returns True if successful, False otherwise
    function Connect(Driver: TJvDriverIndex): Boolean;
    // tries to disconnect from a driver. Returns True if successful, False otherwise
    function Disconnect: Boolean;
    // shows the given dialog and returns True if user pressed ok. If the driver
    // cannot show the given dialog...
    function ShowDialog(Dialog: TJvVideoDialog): Boolean;
    // starts and stop previewing, returning True upon success
    function StartPreview: Boolean;
    function StopPreview: Boolean;
    // start capturing to a file using streaming capture
    function StartCapture: Boolean;
    // start capturing without using a file. You should use the OnVideoStream event in that
    // case to process the frames yourself. This might be useful in a videoconferencing
    // software, where you transfer the frames directly
    function StartCaptureNoFile: Boolean;
    // stops the capture properly
    function StopCapture: Boolean;
    // aborts the capture, leaving the file unusable
    function AbortCapture: Boolean;
    // starts frame by frame capture (non streaming)
    function StartSingleFrameCapture: Boolean;
    // captures one frame in a frame by frame capture session
    function CaptureFrame: Boolean;
    // stops frame by frame capture
    function StopSingleFrameCapture: Boolean;
    // starts and stop overlay rendering, returns True if successful
    function StartOverlay: Boolean;
    function StopOverlay: Boolean;
    // applies the capture settings, returns True if successful
    function ApplyCaptureSettings: Boolean;
    // applies the audio format settings, returns True if successful
    function ApplyAudioFormat: Boolean;
    // saves the stream under the given filename
    function SaveAs(Name: string): Boolean;
    // sets information chunks in the output file
    function SetInfoChunk(const Chunk: TCAPINFOCHUNK): Boolean;
    // saves the latest captured frame to a DIB file
    function SaveDIB(Name: string): Boolean;
    // copies the latest frame to the clipboard
    function CopyToClipboard: Boolean;
    // grabs one frame, not using any capture session
    // if stop is True, previewing and overlaying are stopped
    // if stop is False, previewing and overlaying are left untouched
    function GrabFrame(Stop: Boolean): Boolean;
    // public properties (run-time only), refer to fields and methods descriptions
    // for details on the usage
    property CaptureStatus: TCAPSTATUS read FCaptureStatus;
    property Capturing: Boolean read FCapturing write SetCapturing;
    property Connected: Boolean read FConnected;
    property DriverCaps: TJvDriverCaps read FDriverCaps;
    property DriverName: string read GetDriverName;
    property DriverVersion: string read GetDriverVersion;
    property Drivers: TStrings read GetDrivers;
    property Handle: HWND read FHWnd;
    property Palette: TJvPalette read FPalette;
    property SingleFrameCapturing: Boolean read FSingleFrameCapturing write SetSingleFrameCapturing;
    property VideoFormat: TJvVideoFormat read FVideoFormat;
  published
    // published properties, refer to the field and methods descriptions for details
    property AudioFormat: TJvAudioFormat read FAudioFormat;
    property CaptureSettings: TJvCaptureSettings read FCaptureSettings;
    property DriverIndex: TJvDriverIndex read FDriverIndex write SetDriverIndex default -1;
    property FileName: TFileName read GetFileName write SetFileName;
    property FileSizeAlloc: Cardinal read FFileSizeAlloc write SetFileSizeAlloc default 0;
    property MCIDevice: string read GetMCIDevice write SetMCIDevice;
    property NoFile: Boolean read FNoFile write SetNoFile default False;
    property Overlaying: Boolean read FOverlaying write SetOverlaying default False;
    property PreviewFrameDelay: Cardinal read FPreviewFrameDelay write SetPreviewFrameDelay default 50;
    property PreviewFPS: Double read GetPreviewFPS write SetPreviewFPS;
    property Previewing: Boolean read FPreviewing write SetPreviewing default False;
    property ScrollPos: TJvScrollPos read FScrollPos write SetScrollPos;
    property Title: string read FTitle write SetTitle;
    property UsedEvents: TJvUsedEvents read FUsedEvents write SetUsedEvents default [];
    property VideoLeft: Integer read FVideoLeft write SetVideoLeft default 0;
    property VideoTop: Integer read FVideoTop write SetVideoTop default 0;
    // inherited properties getting published
    property AutoSize;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    // the events, refer to the fields decriptions for details
    property OnError: TOnError read FOnError write FOnError;
    property OnStatus: TOnStatus read FOnStatus write FOnStatus;
    property OnYield: TOnYield read FOnYield write FOnYield;
    property OnFrame: TOnFrame read FOnFrame write FOnFrame;
    property OnVideoStream: TOnVideoStream read FOnVideoStream write FOnVideoStream;
    property OnWaveStream: TOnWaveStream read FOnWaveStream write FOnWaveStream;
    property OnCapControl: TOnCapControl read FOnCapControl write FOnCapControl;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math, // for Min and Max
  JvResources;

const
  // minimal height and width of the display window
  cMinHeight = 20;
  cMinWidth = 20;

{ Global functions }

// an helper function that tells if the window is connected to a driver

function capDriverConnected(hWnd: HWND): Boolean;
var
  TmpName: array [0..MAX_PATH] of Char;
begin
  Result := capDriverGetName(hWnd, TmpName, SizeOf(TmpName));
end;

{ This is the custom window procedure, which replaces the one originally associated
  with the AviCap window. all we do is pass the messages to the TWinControl
  containing the AviCap window so that it can resize and move itself.
  Then we pass the message to the original window procedure for it to handle the
  messages it needs to perform the video capture
}

function CustomWndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  SelfObj: TJvAVICapture;
begin
  Result := 0;

  // get the Pointer to self from the window user data
  SelfObj := TJvAVICapture(GetWindowLong(hWnd, GWL_USERDATA));
  if SelfObj <> nil then
  begin
    // send the message to the containing window,
    // except for WM_NCHITTEST during design
    // This will prevent 100% processor usage when the mouse is kept over
    // the control during design time
    // Note: We MUST convert SelfObj to a TWinControl as the Handle
    // property of TJvAVICapture returns the handle of the AVICap window
    // thus leading to an infinite loop if we were to use it...
    if (Msg <> WM_NCHITTEST) or not (csDesigning in SelfObj.ComponentState) then
        PostMessage(TWinControl(SelfObj).Handle, Msg, wParam, lParam);

    // sending the message to the original window proc
    Result := CallWindowProc(SelfObj.FPreviousWndProc, hWnd, Msg, wParam, lParam);
  end;
end;

{ Callbacks }

// This is the callback called in case of an error
// will only be called if the user chose so with ueError

function ErrorCallback(hWnd: HWND; ErrId: Integer; Str: LPSTR): LRESULT; stdcall;
var
  SelfObj: TJvAVICapture;
begin
  // clear previous error if required
  if ErrId = 0 then
  begin
    Result := LRESULT(Ord(True));
    Exit;
  end;

  // get the Pointer to self from the window user data
  SelfObj := TJvAVICapture(GetWindowLong(hWnd, GWL_USERDATA));
  if SelfObj <> nil then
    SelfObj.DoError(ErrId, Str);

  Result := LRESULT(Ord(True));
end;

// This is the callback called in case of a status change
// will only be called if the user chose so with ueStatus

function StatusCallback(hWnd: HWND; nId: Integer; Str: LPSTR): LRESULT; stdcall;
var
  SelfObj: TJvAVICapture;
begin
  // get the Pointer to self from the window user data
  SelfObj := TJvAVICapture(GetWindowLong(hWnd, GWL_USERDATA));
  if SelfObj <> nil then
    SelfObj.DoStatus(nId, Str);

  Result := LRESULT(Ord(True));
end;

// This is the callback called in case of yielding
// will only be called if the user chose so with ueYield

function YieldCallback(hWnd: HWND): LRESULT; stdcall;
var
  SelfObj: TJvAVICapture;
begin
  // get the Pointer to self from the window user data
  SelfObj := TJvAVICapture(GetWindowLong(hWnd, GWL_USERDATA));
  if SelfObj <> nil then
    SelfObj.DoYield;

  Result := LRESULT(Ord(True));
end;

// This is the callback called in case a new frame is available while a non
// streaming capture is in progress
// will only be called if the user chose so with ueFrame

function FrameCallback(hWnd: HWND; videoHdr: PVIDEOHDR): LRESULT; stdcall;
var
  SelfObj: TJvAVICapture;
begin
  // get the Pointer to self from the window user data
  SelfObj := TJvAVICapture(GetWindowLong(hWnd, GWL_USERDATA));
  if SelfObj <> nil then
    SelfObj.DoFrame(videoHdr);

  Result := LRESULT(Ord(True));
end;

// This is the callback called when a frame is available, just before being
// written to disk, only if using stream capture
// will only be called if the user chose so with ueVideoStream

function VideoStreamCallback(hWnd: HWND; videoHdr: PVIDEOHDR): LRESULT; stdcall;
var
  SelfObj: TJvAVICapture;
begin
  // get the Pointer to self from the window user data
  SelfObj := TJvAVICapture(GetWindowLong(hWnd, GWL_USERDATA));
  if SelfObj <> nil then
    SelfObj.DoVideoStream(videoHdr);

  Result := LRESULT(Ord(True));
end;

// this is the callback when an audio buffer is ready to be written to disk
// and only when using streaming capture
// will only be called if user chose so with ueWaveStream

function WaveStreamCallback(hWnd: HWND; waveHdr: PWaveHdr): LRESULT; stdcall;
var
  SelfObj: TJvAVICapture;
begin
  // get the Pointer to self from the window user data
  SelfObj := TJvAVICapture(GetWindowLong(hWnd, GWL_USERDATA));
  if SelfObj <> nil then
    SelfObj.DoWaveStream(waveHdr);

  Result := LRESULT(Ord(True));
end;

// this is the callback called when a precise capture control event has
// occured. Only called if user chose so with ueCapControl

function CapControlCallback(hWnd: HWND; nState: Integer): LRESULT; stdcall;
var
  SelfObj: TJvAVICapture;
  res: Boolean;
begin
  res := True;
  // get the Pointer to self from the window user data
  SelfObj := TJvAVICapture(GetWindowLong(hWnd, GWL_USERDATA));
  if SelfObj <> nil then
    SelfObj.DoCapControl(nState, res);

  Result := LRESULT(Ord(res));
end;

//=== { TJvVideoFormat } =====================================================

constructor TJvVideoFormat.Create;
begin
  inherited Create;
  FHWnd := 0;
end;

procedure TJvVideoFormat.Update;
var
  BmpInfo: BITMAPINFOHEADER;
begin
  if (FHWnd <> 0) and capDriverConnected(FHWnd) then
  begin
    // get format from the AviCap window
    capGetVideoFormat(FHWnd, @BmpInfo, SizeOf(BmpInfo));

    // update the internal values
    FWidth := BmpInfo.biWidth;
    FHeight := BmpInfo.biHeight;
    FBitDepth := BmpInfo.biBitCount;
    FCompression := BmpInfo.biCompression;

    case BitDepth of
      0:
        FPixelFormat := pfDevice;
      1:
        FPixelFormat := pf1bit;
      4:
        FPixelFormat := pf4bit;
      8:
        FPixelFormat := pf8bit;
      16:
        FPixelFormat := pf15bit;
      24:
        FPixelFormat := pf24bit;
      32:
        FPixelFormat := pf32bit;
    else
      FPixelFormat := pfCustom;
    end;
  end;
end;

//=== { TJvAudioFormat } =====================================================

constructor TJvAudioFormat.Create;
begin
  inherited Create;
  FHWnd := 0;
  FExtra := nil;
end;

procedure TJvAudioFormat.Update;
var
  Info: tWAVEFORMATEX;
begin
  if (FHWnd <> 0) and capDriverConnected(FHWnd) then
  begin
    // gets the format from the AviCap window
    capGetAudioFormat(FHWnd, @Info, SizeOf(Info));

    // sets the internal values
    FFormatTag := Info.wFormatTag;
    FChannels := Info.nChannels;
    FSamplesPerSec := Info.nSamplesPerSec;
    FAvgBytesPerSec := Info.nAvgBytesPerSec;
    FBlockAlign := Info.nBlockAlign;
    FBitsPerSample := Info.wBitsPerSample;
    FExtraSize := Info.cbSize;

    // if there is extra data, save it too
    if FExtraSize > 0 then
    begin
      // if there was extra data saved before, free it before
      if FExtra <> nil then
        FreeMem(FExtra);
      GetMem(FExtra, ExtraSize);
      CopyMemory(FExtra, (PChar(@Info)) + SizeOf(tWAVEFORMATEX), FExtraSize);
    end;
  end;
end;

function TJvAudioFormat.Apply: Boolean;
var
  pwfex: PWaveFormatEx;
begin
  Result := False;
  if FHWnd <> 0 then
  begin
    FillWaveFormatEx(pwfex);
    Result := capSetAudioFormat(FHWnd, pwfex, SizeOf(tWAVEFORMATEX) + pwfex^.cbSize);
  end;
end;

procedure TJvAudioFormat.FillWaveFormatEx(var wfex: PWaveFormatEx);
begin
  case FormatTag of
    WAVE_FORMAT_PCM:
      begin
        GetMem(wfex, SizeOf(tWAVEFORMATEX));
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
    GetMem(wfex, SizeOf(tWAVEFORMATEX) + FExtraSize);
    wfex^.wFormatTag := FFormatTag;
    wfex^.nChannels := FChannels;
    wfex^.nSamplesPerSec := FSamplesPerSec;
    wfex^.nAvgBytesPerSec := FAvgBytesPerSec;
    wfex^.nBlockAlign := FBlockAlign;
    wfex^.wBitsPerSample := FBitsPerSample;
    wfex^.cbSize := FExtraSize;

      // copy Extra to the end of the structure
    CopyMemory((PChar(@wfex)) + SizeOf(tWAVEFORMATEX), FExtra, FExtraSize);
  end;
end;

//=== { TJvCaptureSettings } =================================================

constructor TJvCaptureSettings.Create;
begin
  inherited Create;
  FHWnd := 0;
  FFrameDelay := 1;
end;

procedure TJvCaptureSettings.SetKeyAbort(nKeyAbort: TJvVirtualKey);
var
  Modifiers: Word;
begin
  // Unregister any previous hotkey
  if FKeyAbort <> 0 then
    UnregisterHotKey(FHWnd, 0);

  // register hotkey, only if needed
  if nKeyAbort <> 0 then
  begin
    Modifiers := 0;
    if (nKeyAbort and $4000) <> 0 then
      Modifiers := Modifiers or MOD_SHIFT;
    if (nKeyAbort and $8000) <> 0 then
      Modifiers := Modifiers or MOD_CONTROL;
    if RegisterHotKey(FHWnd, 0, Modifiers, nKeyAbort and $FF) then
      FKeyAbort := nKeyAbort;
  end
  else
    FKeyAbort := nKeyAbort;
end;

procedure TJvCaptureSettings.Update;
var
  Parms: TCAPTUREPARMS;
begin
  if FHWnd <> 0 then
  begin
    // get capture settings from window
    capCaptureGetSetup(FHWnd, @Parms, SizeOf(Parms));

    // udapte internal settings
    with Parms do
    begin
      FFrameDelay := dwRequestMicroSecPerFrame;
//      FFramesPerSec             := 1/dwRequestMicroSecPerFrame*1E6;
      FConfirmCapture := fMakeUserHitOKToCapture;
      FPercentDropForError := wPercentDropForError;
      FYield := FYield;
      FNumVideoBuffer := wNumVideoRequested;
      FCaptureAudio := FCaptureAudio;
      FNumAudioBuffer := wNumAudioRequested;
      FAbortLeftMouse := FAbortLeftMouse;
      FAbortRightMouse := FAbortRightMouse;
      FKeyAbort := vKeyAbort;
      FLimitEnabled := FLimitEnabled;
      FTimeLimit := wTimeLimit;
      FStepCapture2x := fStepCaptureAt2x;
      FStepCaptureAverageFrames := wStepCaptureAverageFrames;
      FAudioBufferSize := dwAudioBufferSize;
      FAudioMaster := (AVStreamMaster = AVSTREAMMASTER_AUDIO);
      FMCIControl := FMCIControl;
      FMCIStep := fStepMCIDevice;
      FMCIStartTime := dwMCIStartTime;
      FMCIStopTime := dwMCIStopTime;
    end;
  end;
end;

function TJvCaptureSettings.Apply: Boolean;
var
  Parms: TCAPTUREPARMS;
begin
  Result := False;
  if FHWnd <> 0 then
  begin
    // get original values from window
    capCaptureGetSetup(FHWnd, @Parms, SizeOf(Parms));

    // set our own values
    with Parms do
    begin
      dwRequestMicroSecPerFrame := FFrameDelay;
      fMakeUserHitOKToCapture := ConfirmCapture;
      wPercentDropForError := PercentDropForError;
      FYield := Yield;
      wNumVideoRequested := NumVideoBuffer;
      FCaptureAudio := CaptureAudio;
      wNumAudioRequested := NumAudioBuffer;
      FAbortLeftMouse := AbortLeftMouse;
      FAbortRightMouse := AbortRightMouse;
      vKeyAbort := FKeyAbort;
      FLimitEnabled := LimitEnabled;
      wTimeLimit := TimeLimit;
      fStepCaptureAt2x := StepCapture2x;
      wStepCaptureAverageFrames := StepCaptureAverageFrames;
      dwAudioBufferSize := AudioBufferSize;
      if AudioMaster then
        AVStreamMaster := AVSTREAMMASTER_AUDIO
      else
        AVStreamMaster := AVSTREAMMASTER_NONE;
      FMCIControl := Self.FMCIControl;
      fStepMCIDevice := Self.FMCIStep;
      dwMCIStartTime := FMCIStartTime;
      dwMCIStopTime := FMCIStopTime;
    end;

    // apply new settings
    Result := capCaptureSetSetup(FHWnd, @Parms, SizeOf(Parms));
  end;
end;

function TJvCaptureSettings.GetFPS: Double;
begin
  Result := 1 / FFrameDelay * 1.0E6;
end;

procedure TJvCaptureSettings.SetFPS(const Value: Double);
begin
  FFrameDelay := Round(1.0E6 / Value);
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

//=== { TJvPalette } =========================================================

constructor TJvPalette.Create;
begin
  inherited Create;
  FHWnd := 0;
end;

function TJvPalette.Load(FileName: string): Boolean;
begin
  Result := (FHWnd <> 0) and capPaletteOpen(FHWnd, PChar(FileName));
end;

function TJvPalette.Save(FileName: string): Boolean;
begin
  Result := (FHWnd <> 0) and capPaletteSave(FHWnd, PChar(FileName));
end;

function TJvPalette.PasteFromClipboard: Boolean;
begin
  Result := (FHWnd <> 0) and capPalettePaste(FHWnd);
end;

function TJvPalette.AutoCreate(nbFrames: Integer; nbColors: TJvPaletteNbColors): Boolean;
begin
  Result := (FHWnd <> 0) and capPaletteAuto(FHWnd, nbFrames, nbColors);
end;

function TJvPalette.ManuallyCreate(Flag: Boolean; nbColors: TJvPaletteNbColors): Boolean;
begin
  Result := (FHWnd <> 0) and capPaletteManual(FHWnd, Flag, nbColors);
end;

//=== { TJvAVICapture } ======================================================

constructor TJvAVICapture.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScrollPos := TJvScrollPos.Create;
  // Not connected yet
  FDriverIndex := -1;
  FFileSizeAlloc := 0;
  FOverlaying := False;
  FPreviewing := False;
  FUsedEvents := [];
  FVideoLeft := 0;
  FVideoTop := 0;
  FDrivers := TStringList.Create;
  // Preview frame delay = 50ms between frames (20 frames per second)
  FPreviewFrameDelay := 50;
  FVideoFormat := TJvVideoFormat.Create;
  FAudioFormat := TJvAudioFormat.Create;
  // Default to PCM, 11.025khz 8 bit Mono
  with FAudioFormat do
  begin
    FormatTag := WAVE_FORMAT_PCM;
    Channels := 1;
    BitsPerSample := 8;
    SamplesPerSec := 11025;
  end;
  FCaptureSettings := TJvCaptureSettings.Create;
  FPalette := TJvPalette.Create;
  SetBounds(0, 0, 320, 240);
  EnumDrivers;
  // set all events to 'used'
  UsedEvents := [ueError, ueStatus, ueYield, ueFrame, ueVideoStream, ueWaveStream, ueCapControl];
end;

destructor TJvAVICapture.Destroy;
begin
  Disconnect;
  FDrivers.Free;
  FCaptureSettings.Free;
  FAudioFormat.Free;
  FVideoFormat.Free;
  FPalette.Free;
  FScrollPos.Free;
  inherited Destroy;
end;

procedure TJvAVICapture.CreateWindowHandle(const Params: TCreateParams);
begin
  // ensure the TWinControl is fully created first
  inherited CreateWindowHandle(Params);
  // no hint to show
  //ParentShowHint := False;
  //ShowHint := False;

  // create the AviCap window
  FHWnd := capCreateCaptureWindow(
    PChar(Title),        // use the user defined title
    WS_VISIBLE or        // window is visible
      WS_CHILD and       // it is a child window
      not WS_CAPTION and // it has no caption
      not WS_BORDER,     // it has no border
    0,                   // 0 left coordinate
    0,                   // 0 top coordinate
    320,                 // width defaults to 320
    240,                 // height defaults to 240
    inherited Handle,    // child of the TWinControl
    0);                  // window identifier

  // place the Pointer to Self in the user data
  SetWindowLong(FHWnd, GWL_USERDATA, Integer(Self));
  // replace the WndProc to be ours
  FPreviousWndProc := Pointer(GetWindowLong(FHWnd, GWL_WNDPROC));
  SetWindowLong(FHWnd, GWL_WNDPROC, Integer(@CustomWndProc));
  // updates the FHWnd member of audio format, capture settings, palette and video format
  // yes, they are private members, but they can still be accessed by a foreign class
  // because the access is done in the same pas file !
  FAudioFormat.FHWnd := FHWnd;
  FCaptureSettings.FHWnd := FHWnd;
  FPalette.FHWnd := FHWnd;
  FVideoFormat.FHWnd := FHWnd;
  // sets the callbacks
  UsedEvents := FUsedEvents;
end;

procedure TJvAVICapture.DestroyWindowHandle;
begin
  // restore the window proc
  SetWindowLong(FHWnd, GWL_WNDPROC, Integer(FPreviousWndProc));
  // destroy the AviCap Window
  DestroyWindow(FHWnd);
  // let the TWinControl window be destroyed
  inherited DestroyWindowHandle;
end;

procedure TJvAVICapture.SetTitle(nTitle: string);
begin
  if FHWnd <> 0 then
  begin
    FTitle := nTitle;
    SetWindowText(FHWnd, PChar(FTitle));
  end;
end;

procedure TJvAVICapture.SetPreviewFrameDelay(nPreviewFrameDelay: Cardinal);
begin
  FPreviewFrameDelay := nPreviewFrameDelay;
  if Previewing then
  begin
    StopPreview;
    StartPreview;
  end;
end;

procedure TJvAVICapture.SetPreviewFPS(nPreviewFPS: Double);
begin
  SetPreviewFrameDelay(Round(1.0E3 * 1.0 / nPreviewFPS));
end;

function TJvAVICapture.GetPreviewFPS: Double;
begin
  Result := 1.0E3 * 1.0 / FPreviewFrameDelay;
end;

procedure TJvAVICapture.SetPreviewing(nPreviewing: Boolean);
begin
  if (not nPreviewing) and Previewing then
    StopPreview;
  if nPreviewing and (not Previewing) then
    StartPreview;
end;

procedure TJvAVICapture.SetFileName(nFileName: TFileName);
begin
  if FHWnd <> 0 then
  begin
    FFileName := nFileName;
    // change the filename
    capFileSetCaptureFile(FHWnd, PChar(nFileName));
  end;
end;

function TJvAVICapture.GetFileName: TFileName;
var
  Name: array [0..MAX_PATH] of Char;
begin
  if FHWnd <> 0 then
  begin
    // get the filename from the window
    capFileGetCaptureFile(FHWnd, Name, SizeOf(Name));
    FFileName := Name;
  end;
  Result := FFileName;
end;

function TJvAVICapture.GetDrivers: TStrings;
begin
  Result := FDrivers;
end;

procedure TJvAVICapture.SetFileSizeAlloc(nFileSizeAlloc: Cardinal);
begin
  if FHWnd <> 0 then
  begin
    FFileSizeAlloc := nFileSizeAlloc;
    capFileAlloc(FHWnd, FFileSizeAlloc);
  end;
end;

procedure TJvAVICapture.SetUsedEvents(nUsedEvents: TJvUsedEvents);
begin
  FUsedEvents := nUsedEvents;

  if FHWnd <> 0 then
  begin
    if ueError in FUsedEvents then
      capSetCallbackOnError(FHWnd, @ErrorCallback)
    else
      capSetCallbackOnError(FHWnd, nil);

    if ueStatus in FUsedEvents then
      capSetCallbackOnStatus(FHWnd, @StatusCallback)
    else
      capSetCallbackOnStatus(FHWnd, nil);

    if ueYield in FUsedEvents then
      capSetCallbackOnYield(FHWnd, @YieldCallback)
    else
      capSetCallbackOnYield(FHWnd, nil);

    if ueFrame in FUsedEvents then
      capSetCallbackOnFrame(FHWnd, @FrameCallback)
    else
      capSetCallbackOnFrame(FHWnd, nil);

    if ueVideoStream in FUsedEvents then
      capSetCallbackOnVideoStream(FHWnd, @VideoStreamCallback)
    else
      capSetCallbackOnVideoStream(FHWnd, nil);

    if ueWaveStream in FUsedEvents then
      capSetCallbackOnWaveStream(FHWnd, @WaveStreamCallback)
    else
      capSetCallbackOnWaveStream(FHWnd, nil);

    if ueCapControl in FUsedEvents then
      capSetCallbackOnCapControl(FHWnd, @CapControlCallback)
    else
      capSetCallbackOnCapControl(FHWnd, nil);
  end;
end;

procedure TJvAVICapture.SetOverlaying(nOverlaying: Boolean);
begin
  if not nOverlaying then
  begin
    if Overlaying then
      StopOverlay;
  end
  else
  if not Overlaying then
    StartOverlay;
end;

function TJvAVICapture.GetDriverName: string;
var
  Name: array [0..MAX_PATH] of Char;
begin
  if FHWnd <> 0 then
  begin
    capDriverGetName(FHWnd, Name, SizeOf(Name));
    Result := Name;
  end
  else
    Result := RsNotConnected;
end;

function TJvAVICapture.GetDriverVersion: string;
var
  Version: array [0..MAX_PATH] of Char;
begin
  if FHWnd <> 0 then
  begin
    capDriverGetVersion(FHWnd, Version, SizeOf(Version));
    Result := Version;
  end
  else
    Result := RsNotConnected;
end;

procedure TJvAVICapture.SetScrollPos(nScrollPos: TJvScrollPos);
var
  TmpPoint: TPoint;
begin
  if FHWnd <> 0 then
  begin
    FScrollPos := nScrollPos;
    TmpPoint.X := FScrollPos.Left;
    TmpPoint.Y := FScrollPos.Top;
    capSetScrollPos(FHWnd, @TmpPoint);
  end;
end;

procedure TJvAVICapture.SetMCIDevice(nMCIDevice: string);
begin
  if FHWnd <> 0 then
    capSetMCIDeviceName(FHWnd, PChar(nMCIDevice));
end;

function TJvAVICapture.GetMCIDevice: string;
var
  Name: array [0..MAX_PATH] of Char;
begin
  if FHWnd <> 0 then
  begin
    capGetMCIDeviceName(FHWnd, Name, SizeOf(Name));
    Result := Name;
  end
  else
    Result := RsNotConnected;
end;

procedure TJvAVICapture.SetDriverIndex(nIndex: TJvDriverIndex);
begin
  if Connect(nIndex) then
    FDriverIndex := nIndex;
end;

procedure TJvAVICapture.SetCapturing(nCapturing: Boolean);
begin
  if FCapturing then
  begin
    if not nCapturing then
      StopCapture;
  end
  else
  if nCapturing then
    if FNoFile then
      StartCaptureNoFile
    else
      StartCapture;
end;

procedure TJvAVICapture.SetNoFile(nNoFile: Boolean);
begin
  // only allow to change if not capturing
  if not FCapturing then
    FNoFile := nNoFile;
end;

procedure TJvAVICapture.UpdateCaps;
var
  Caps: TCAPDRIVERCAPS;
begin
  if FHWnd <> 0 then
  begin
    // get value from the window
    capDriverGetCaps(FHWnd, @Caps, SizeOf(Caps));
    // update internal value
    FDriverCaps := [];
    if Caps.fHasOverlay then
      FDriverCaps := FDriverCaps + [dcOverlay];
    if Caps.fHasDlgVideoSource then
      FDriverCaps := FDriverCaps + [dcDlgVideoSource];
    if Caps.fHasDlgVideoFormat then
      FDriverCaps := FDriverCaps + [dcDlgVideoFormat];
    if Caps.fHasDlgVideoDisplay then
      FDriverCaps := FDriverCaps + [dcDlgVideoDisplay];
    if Caps.fCaptureInitialized then
      FDriverCaps := FDriverCaps + [dcCaptureInitialized];
    if Caps.fDriverSuppliesPalettes then
      FDriverCaps := FDriverCaps + [dcSuppliesPalettes];
  end;
end;

procedure TJvAVICapture.UpdateCaptureStatus;
begin
  if FHWnd <> 0 then
  begin
    capGetStatus(FHWnd, @FCaptureStatus, SizeOf(FCaptureStatus));
    FCapturing := FCaptureStatus.fCapturingNow;
    FPreviewing := FCaptureStatus.fLiveWindow;
    FOverlaying := FCaptureStatus.fOverlayWindow;
  end;
end;

procedure TJvAVICapture.StopCallbacks;
begin
  if FHWnd <> 0 then
  begin
    if not (csDesigning in ComponentState) then
      capSetCallbackOnError(FHWnd, nil);

    capSetCallbackOnStatus(FHWnd, nil);
    capSetCallbackOnYield(FHWnd, nil);
    capSetCallbackOnFrame(FHWnd, nil);
    capSetCallbackOnVideoStream(FHWnd, nil);
    capSetCallbackOnWaveStream(FHWnd, nil);
    capSetCallbackOnCapControl(FHWnd, nil);
  end;
end;

procedure TJvAVICapture.RestartCallbacks;
begin
  UsedEvents := FUsedEvents;
end;

procedure TJvAVICapture.SetBounds(nLeft, nTop, nWidth, nHeight: Integer);
var
  lWidth, lHeight: Integer;
begin
  // reload video size
  FVideoFormat.Update;

  // else, force the width and height to stay in a constant interval :
  // not less than cMinHeight and cMinWidth
  // not more than the video size
  // Autosizing will have been enforced in the CanAutoSize procedure
  lHeight := Max(Min(nHeight, FVideoFormat.Height), cMinHeight);
  lWidth := Max(Min(nWidth, FVideoFormat.Width), cMinWidth);

  inherited SetBounds(nLeft, nTop, lWidth, lHeight);
end;

procedure TJvAVICapture.EnumDrivers;
var
  I: Integer;
  DeviceName: array [0..MAX_PATH] of Char;
  DeviceVersion: array [0..MAX_PATH] of Char;
begin
  // no more than 10 drivers in the system (cf Win32 API)
  for I := 0 to 9 do
    if capGetDriverDescription(I, DeviceName, SizeOf(DeviceName), DeviceVersion, SizeOf(DeviceVersion)) then
      Drivers.Add(DeviceName);
end;

function TJvAVICapture.Connect(Driver: TJvDriverIndex): Boolean;
begin
  // Request a handle, will create the AviCap internal window
  // will trigger an exception if no parent is set
  HandleNeeded;

  if Driver = -1 then
  begin
    // if Driver is -1, then we disconnect
    Result := Disconnect;
    // force the video format to be 0, 0 and update the size of the control
    FVideoFormat.FHeight := 0;
    FVideoFormat.FWidth := 0;
  end
  else
  begin
    // else we try to connect to that driver
    Result := capDriverConnect(FHWnd, Driver);
    FConnected := Result;

    if FConnected then
    begin
      // if connected successfully, update the property
      FDriverIndex := Driver;
      UpdateCaps;
      FCaptureSettings.Update;
      FAudioFormat.Update;
      UpdateCaptureStatus;

    end
    else
      // if not, trigger an exception
      raise EInvalidDriverIndexError.Create(Driver, Drivers.Count - 1);
  end;
  AdjustSize;
end;

function TJvAVICapture.Disconnect: Boolean;
begin
  Result := capDriverDisconnect(FHWnd);
  UpdateCaptureStatus;
  FConnected := False;
end;

function TJvAVICapture.ShowDialog(Dialog: TJvVideoDialog): Boolean;
begin
  Result := False;
  if FHWnd <> 0 then
  begin
    case Dialog of
      vdSource:
        Result := capDlgVideoSource(FHWnd);
      vdFormat:
        Result := capDlgVideoFormat(FHWnd);
      vdDisplay:
        Result := capDlgVideoDisplay(FHWnd);
      vdCompression:
        Result := capDlgVideoCompression(FHWnd);
    end;
    // update everything to reflect user changes
    UpdateCaps;
    VideoFormat.Update;
    AudioFormat.Update;
    CaptureSettings.Update;
    SetBounds(Left, Top, Width, Height);
  end;
end;

function TJvAVICapture.StartPreview: Boolean;
begin
  // if we have a valid window that is not already previewing
  if (FHWnd <> 0) and not FPreviewing then
  begin
    capPreviewRate(FHWnd, FPreviewFrameDelay);
    FPreviewing := capPreview(FHWnd, True);
    UpdateCaptureStatus;
    VideoFormat.Update;
    if FPreviewing then
    begin
      FOverlaying := False;
      RestartCallbacks;
    end;
    Result := FPreviewing;
  end
  else
    Result := False;
end;

function TJvAVICapture.StopPreview: Boolean;
begin
  // if we have a valid window doing previewing
  // then the result is the result of capPreview
  Result := (FHWnd <> 0) and FPreviewing and capPreview(FHWnd, False);

  // if succesfully stopped preview, update internal values
  if Result then
  begin
    UpdateCaptureStatus;
    FPreviewing := False;
    StopCallbacks;
  end;
end;

function TJvAVICapture.StartCapture: Boolean;
begin
  if (FHWnd <> 0) and not FCapturing and ApplyCaptureSettings and
    ApplyAudioFormat then
  begin
    UpdateCaptureStatus;
    VideoFormat.Update;
    FCapturing := capCaptureSequence(FHWnd);
    if FCapturing then
      RestartCallbacks;
    Result := FCapturing;
  end
  else
    Result := False;
end;

function TJvAVICapture.StartCaptureNoFile: Boolean;
begin
  if (FHWnd <> 0) and not FCapturing and ApplyCaptureSettings and
    ApplyAudioFormat then
  begin
    UpdateCaptureStatus;
    VideoFormat.Update;
    FCapturing := capCaptureSequenceNoFile(FHWnd);
    FNoFile := True;
    if FCapturing then
      RestartCallbacks;
    Result := FCapturing;
  end
  else
    Result := False;
end;

function TJvAVICapture.StopCapture: Boolean;
begin
  Result := (FHWnd <> 0) and FCapturing and capCaptureStop(FHWnd);
  if Result then
  begin
    FCapturing := False;
    StopCallbacks;
  end;
end;

function TJvAVICapture.AbortCapture: Boolean;
begin
  Result := (FHWnd <> 0) and FCapturing and capCaptureAbort(FHWnd);
  if Result then
  begin
    FCapturing := False;
    StopCallbacks;
  end;
end;

function TJvAVICapture.StartSingleFrameCapture: Boolean;
begin
  Result := (FHWnd <> 0) and not FSingleFrameCapturing and
    capCaptureSingleFrameOpen(FHWnd);
  if Result then
  begin
    UpdateCaptureStatus;
    VideoFormat.Update;
    RestartCallbacks;
    FSingleFrameCapturing := True;
  end;
end;

function TJvAVICapture.CaptureFrame: Boolean;
begin
  Result := (FHWnd <> 0) and FSingleFrameCapturing and
    capCaptureSingleFrame(FHWnd);
  UpdateCaptureStatus;
  VideoFormat.Update;
end;

function TJvAVICapture.StopSingleFrameCapture: Boolean;
begin
  Result := (FHWnd <> 0) and FSingleFrameCapturing and
    capCaptureSingleFrameClose(FHWnd);
  if Result then
  begin
    UpdateCaptureStatus;
    VideoFormat.Update;
    StopCallbacks;
    FSingleFrameCapturing := False;
  end;
end;

function TJvAVICapture.StartOverlay: Boolean;
begin
  if (FHWnd <> 0) and not FOverlaying then
  begin
    capPreviewRate(FHWnd, FPreviewFrameDelay);
    FOverlaying := capOverlay(FHWnd, True);
    UpdateCaptureStatus;
    VideoFormat.Update;
    if FOverlaying then
    begin
      FPreviewing := False;
      RestartCallbacks;
    end;
    Result := FOverlaying;
  end
  else
    Result := False;
end;

function TJvAVICapture.StopOverlay: Boolean;
begin
  Result := (FHWnd <> 0) and FOverlaying and capOverlay(FHWnd, False);
  if Result then
  begin
    UpdateCaptureStatus;
    FOverlaying := False;
    StopCallbacks;
  end;
end;

function TJvAVICapture.ApplyCaptureSettings: Boolean;
begin
  Result := CaptureSettings.Apply;
end;

function TJvAVICapture.ApplyAudioFormat: Boolean;
begin
  Result := AudioFormat.Apply;
end;

function TJvAVICapture.SaveAs(Name: string): Boolean;
begin
  Result := (FHWnd <> 0) and capFileSaveAs(FHWnd, PChar(Name));
end;

function TJvAVICapture.SetInfoChunk(const Chunk: TCAPINFOCHUNK): Boolean;
begin
  Result := (FHWnd <> 0) and capFileSetInfoChunk(FHWnd, @Chunk);
end;

function TJvAVICapture.SaveDIB(Name: string): Boolean;
begin
  Result := (FHWnd <> 0) and capFileSaveDIB(FHWnd, PChar(Name));
end;

function TJvAVICapture.CopyToClipboard: Boolean;
begin
  Result := (FHWnd <> 0) and capEditCopy(FHWnd);
end;

function TJvAVICapture.GrabFrame(Stop: Boolean): Boolean;
begin
  Result := False;
  if FHWnd <> 0 then
    if Stop then
    begin
      FPreviewing := False;
      FOverlaying := False;
      Result := capGrabFrame(FHWnd);
    end
    else
      Result := capGrabFrameNoStop(FHWnd);
end;

procedure TJvAVICapture.DoError(ErrId: Integer; Str: string);
begin
  if csDesigning in ComponentState then
    Windows.MessageBox(WindowHandle, PChar(Str), PChar(RsErrorMessagePrefix + IntToStr(ErrId)), MB_ICONERROR);
  if Assigned(FOnError) then
    FOnError(Self, ErrId, Str);
end;

procedure TJvAVICapture.DoStatus(nId: Integer; Str: string);
begin
  UpdateCaptureStatus;
  if Assigned(FOnStatus) then
    FOnStatus(Self, nId, Str);
end;

procedure TJvAVICapture.DoYield;
begin
  UpdateCaptureStatus;
  if Assigned(FOnYield) then
    FOnYield(Self);
end;

procedure TJvAVICapture.DoFrame(videoHdr: PVIDEOHDR);
begin
  if Assigned(FOnFrame) then
    FOnFrame(Self, videoHdr);
end;

procedure TJvAVICapture.DoVideoStream(videoHdr: PVIDEOHDR);
begin
  if Assigned(FOnVideoStream) then
    FOnVideoStream(Self, videoHdr);
end;

procedure TJvAVICapture.DoWaveStream(waveHdr: PWaveHdr);
begin
  if Assigned(FOnWaveStream) then
    FOnWaveStream(Self, waveHdr);
end;

procedure TJvAVICapture.DoCapControl(nState: Integer; var AResult: Boolean);
begin
  AResult := True;
  if Assigned(FOnCapControl) then
    FOnCapControl(Self, nState, AResult);
end;

procedure TJvAVICapture.SetVideoLeft(const Value: Integer);
var
  P: TPoint;
begin
  P.X := Value;
  P.Y := VideoTop;
  if capSetScrollPos(FHWnd, @P) then
    FVideoLeft := Value;
end;

procedure TJvAVICapture.SetVideoTop(const Value: Integer);
var
  P: TPoint;
begin
  P.X := VideoLeft;
  P.Y := Value;
  if capSetScrollPos(FHWnd, @P) then
    FVideoTop := Value;
end;

function TJvAVICapture.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  // always possible to do autosizing
  Result := True;

  // reload video size
  FVideoFormat.Update;

  // force the width and height to be equal
  // to the one from the video (with a minimum value set
  // in case there is no video yet)
  NewHeight := Max(cMinHeight, FVideoFormat.Height);
  NewWidth := Max(cMinWidth, FVideoFormat.Width);
end;

procedure TJvAVICapture.SetSingleFrameCapturing(const Value: Boolean);
begin
  if Value then
    StartSingleFrameCapture
  else
    StopSingleFrameCapture;
end;

//=== EInvalidDriverIndexError ===============================================

constructor EInvalidDriverIndexError.Create(Index: TJvDriverIndex; MaxIndex: TJvDriverIndex);
begin
  inherited CreateFmt(RsEInvalidDriverIndex, [Index, MaxIndex]);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.


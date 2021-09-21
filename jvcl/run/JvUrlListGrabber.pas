{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUrlListGrabber.Pas, released on 2003-08-04.

The Initial Developer of the Original Code is Olivier Sannier [obones att altern dott org]
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvUrlListGrabber;

interface

{$I jvcl.inc}

{$IFDEF WIN64}
{$HPPEMIT '#pragma link "wininet.a"'}
{$ELSE}
{$HPPEMIT '#pragma link "wininet.lib"'}
{$ENDIF WIN64}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, WinInet, Classes, SysUtils, Contnrs,
  JvComponentBase, JvTypes;

type
  // early declarations
  TJvUrlListGrabber = class;
  TJvCustomUrlGrabber = class;
  TJvUrlGrabberList = class;
  TJvUrlGrabberDefaultPropertiesList = class;

  // A Grabber index, defined as a new type to allow to give it
  // a specific property editor
  TJvUrlGrabberIndex = type Integer;

  // Timeout values
  // -1 means do not change the current option.
  //  0 means infinite
  // Any other positive value is timeout in milliseconds.
  TJvUrlGrabberTimeOut = class(TPersistent)
  private
    FConnect: Integer;
    FReceive: Integer;
    FSend: Integer;
  public
    constructor Create;

    procedure Assign(Source : TPersistent); override;
    procedure SetupSession(ASession: HINTERNET);
  published
    property Connect: Integer read FConnect write FConnect default -1;
    property Receive: Integer read FReceive write FReceive default -1;
    property Send: Integer read FSend write FSend default -1;
  end;

  // The event triggered when a new grabber are created/added
  TJvGrabberCreatedEvent = procedure(Sender: TJvUrlListGrabber; Grabber: TJvCustomUrlGrabber) of object;
  TJvGrabberAddedEvent = procedure(Sender: TJvUrlListGrabber; Grabber: TJvCustomUrlGrabber; Index: Integer) of object;

  // The type of the events triggered when one of the grabbers
  // has triggered its own event to indicate a change in its state
  TJvGrabberNotifyEvent = procedure(Sender: TJvUrlListGrabber; Grabber: TJvCustomUrlGrabber) of object;

  // Set of type of events triggered by TJvUrlListGrabber to indicate that
  // one of its grabbers has triggered the corresponding event
  TJvGrabberDoneFileEvent = procedure(Sender: TJvUrlListGrabber; Grabber: TJvCustomUrlGrabber; FileName: string;
    FileSize: Integer; Url: string) of object;
  TJvGrabberDoneStreamEvent = procedure(Sender: TJvUrlListGrabber; Grabber: TJvCustomUrlGrabber; Stream: TStream;
    StreamSize: Integer; Url: string) of object;
  TJvGrabberProgressEvent = procedure(Sender: TJvUrlListGrabber; Grabber: TJvCustomUrlGrabber; Position, TotalSize:
    Int64; Url: string; var Continue: Boolean) of object;
  TJvGrabberErrorEvent = procedure(Sender: TJvUrlListGrabber; Grabber: TJvCustomUrlGrabber; ErrorMsg: string) of object;

  // The exception raised by TJvUrlListGrabber when no grabber claimed it was capable
  // of handling a given URL. This is only raised if DefaultGrabberIndex is -1
  ENoGrabberForUrl = class(Exception);

  // The exception triggered if someone tries to set the URLs property while at
  // least one grabber is running
  EAtLeastOneGrabberRunning = class(Exception);

  // This component allows the user to specify a list of URLs to be
  // grabbed and then start grabbing. All the grab operations will be done
  // in parallel in the background, leaving the user's application free
  // to continue its operations
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvUrlListGrabber = class(TJvComponent)
  private
    FOnDoneFile: TJvGrabberDoneFileEvent;
    FOnDoneStream: TJvGrabberDoneStreamEvent;
    FOnError: TJvGrabberErrorEvent;
    FOnProgress: TJvGrabberProgressEvent;
    FOnConnectionClosed: TJvGrabberNotifyEvent;
    FOnReceivingResponse: TJvGrabberNotifyEvent;
    FOnRequestComplete: TJvGrabberNotifyEvent;
    FOnResponseReceived: TJvGrabberNotifyEvent;
    FOnConnectingToServer: TJvGrabberNotifyEvent;
    FOnResolvingName: TJvGrabberNotifyEvent;
    FOnClosingConnection: TJvGrabberNotifyEvent;
    FOnConnectedToServer: TJvGrabberNotifyEvent;
    FOnRedirect: TJvGrabberNotifyEvent;
    FOnNameResolved: TJvGrabberNotifyEvent;
    FOnSendingRequest: TJvGrabberNotifyEvent;
    FOnRequestSent: TJvGrabberNotifyEvent;
    FOnStatusChange: TJvGrabberNotifyEvent;
    FOnGrabberCreated: TJvGrabberCreatedEvent;

    FCleanupThreshold: Cardinal;
    FCleanupList: TObjectList;
    FGrabbers: TJvUrlGrabberList;
    FURLs: TStringList;
    FDefaultGrabberIndex: TJvUrlGrabberIndex;
    FDefaultGrabbersProperties: TJvUrlGrabberDefaultPropertiesList;
    FMaxSimultaneousGrabbers: Integer;
    FNextURLIndex: Integer;
    FOnGrabberAdded: TJvGrabberAddedEvent;

    FTimeOut : TJvUrlGrabberTimeOut;

    // gets/sets the URLs property, assigning the given strings
    // to the internal FURLs field
    function GetURLs: TStrings;
    procedure SetURLs(const Value: TStrings);

    // sets the Default Grabber value, ensuring that it doesn't go
    // below -1 or above the number of registered grabber classes
    // if you try to set the value above the last index in the
    // JvUrlGrabberClassList, then the value will be set to -1.
    // The same goes if you set a value below -1.
    procedure SetDefaultGrabberIndex(const Value: TJvUrlGrabberIndex);

    // returns the grabber associated with the given index
    function GetGrabbers(const Index: Integer): TJvCustomUrlGrabber;

    // Called whenever the list of Urls is about to change
    procedure URLsChanging(Sender: TObject);

    // The event handlers for the grabbers, to propagate them to the
    // user through the events of this class
    procedure GrabberDoneFile(Grabber: TObject; FileName: string; FileSize: Integer; Url: string);
    procedure GrabberDoneStream(Grabber: TObject; Stream: TStream; StreamSize: Integer; Url: string);
    procedure GrabberProgress(Grabber: TObject; Position, TotalSize: Int64; Url: string; var Continue: Boolean);
    procedure GrabberError(Grabber: TObject; ErrorMsg: string);
    procedure GrabberConnectionClosed(Grabber: TObject);
    procedure GrabberReceivingResponse(Grabber: TObject);
    procedure GrabberRequestComplete(Grabber: TObject);
    procedure GrabberResponseReceived(Grabber: TObject);
    procedure GrabberConnectingToServer(Grabber: TObject);
    procedure GrabberResolvingName(Grabber: TObject);
    procedure GrabberClosingConnection(Grabber: TObject);
    procedure GrabberConnectedToServer(Grabber: TObject);
    procedure GrabberRedirect(Grabber: TObject);
    procedure GrabberNameResolved(Grabber: TObject);
    procedure GrabberSendingRequest(Grabber: TObject);
    procedure GrabberRequestSent(Grabber: TObject);
    procedure GrabberStatusChange(Grabber: TObject);
    function GetGrabberCount: Integer;
    procedure SetMaxSimultaneousGrabbers(const Value: Integer);
  protected
    // Sets the events of the given grabber to call the internal
    // event handlers indicated below. This way, the events of
    // TJvUrlListGrabber will be triggered properly
    procedure SetGrabberEvents(Grabber: TJvCustomUrlGrabber);

    // Returns a new grabber for the given URL or raises an exception if
    // no grabber could be found for the URL.
    // Note: The events of the returned grabber are not set.
    function GetGrabberForUrl(const URL: string): TJvCustomUrlGrabber;

    procedure DoGrabberCreated(Grabber: TJvCustomUrlGrabber);
    procedure DoGrabberAdded(Grabber: TJvCustomUrlGrabber; Index: Integer);

    procedure StartNextGrabber;

    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // cleans up the internal list of grabbers
    procedure Cleanup;

    // starts all the grabbers. Deprecated, use Start instead.
    procedure StartAll; {$IFDEF SUPPORTS_DEPRECATED}deprecated;{$ENDIF SUPPORTS_DEPRECATED}
    // stops all the grabbers. Deprecated, use Stop instead.
    procedure StopAll; {$IFDEF SUPPORTS_DEPRECATED}deprecated;{$ENDIF SUPPORTS_DEPRECATED}

    procedure Start;
    procedure Stop;

    // The Grabber objects associated with the Urls. This array contains up
    // to FUrls.Count if MaxSimultaneousGrabbers is 0, else up to the value
    // of MaxSimultaneousGrabbers. Note that this array only contains elements
    // if at least one URL is being grabbed. Hence, before you call StartAll,
    // it is always empty.
    property Grabbers[const Index: Integer]: TJvCustomUrlGrabber read GetGrabbers;
    property GrabberCount: Integer read GetGrabberCount;
    property NexUrlIndex: Integer read FNextUrlIndex;
  published
    // the index of the default grabber to use, if any
    property DefaultGrabberIndex: TJvUrlGrabberIndex read FDefaultGrabberIndex write SetDefaultGrabberIndex default -1;

    // The cleanup threshold. When a grabber has finished grabbing it is placed
    // in the "Cleanup" list. The grabber cannot be destroyed immediately as
    // events may still be trigerred for it. Hence it is placed in the list
    // and this list is not emptied every time but only when it contains more
    // elements than the value of CleanupThreshold.
    property CleanupThreshold: Cardinal read FCleanupThreshold write FCleanupThreshold default 10;

    // Maximum number of grabbers running simultaneously. 0 means no limit.
    property MaxSimultaneousGrabbers: Integer read FMaxSimultaneousGrabbers write SetMaxSimultaneousGrabbers default 0;

    // Timeout options
    property TimeOut: TJvUrlGrabberTimeOut read FTimeOut;

    // The Urls to grab
    property URLs: TStrings read GetURLs write SetURLs;
    // The default properties for each family of grabber
    property DefaultGrabbersProperties: TJvUrlGrabberDefaultPropertiesList read FDefaultGrabbersProperties;

    // Events from Grabbers
    property OnDoneFile: TJvGrabberDoneFileEvent read FOnDoneFile write FOnDoneFile;
    property OnDoneStream: TJvGrabberDoneStreamEvent read FOnDoneStream write FOnDoneStream;
    property OnError: TJvGrabberErrorEvent read FOnError write FOnError;
    property OnProgress: TJvGrabberProgressEvent read FOnProgress write FOnProgress;
    property OnResolvingName: TJvGrabberNotifyEvent read FOnResolvingName write FOnResolvingName;
    property OnNameResolved: TJvGrabberNotifyEvent read FOnNameResolved write FOnNameResolved;
    property OnConnectingToServer: TJvGrabberNotifyEvent read FOnConnectingToServer write FOnConnectingToServer;
    property OnConnectedToServer: TJvGrabberNotifyEvent read FOnConnectedToServer write FOnConnectedToServer;
    property OnSendingRequest: TJvGrabberNotifyEvent read FOnSendingRequest write FOnSendingRequest;
    property OnRequestSent: TJvGrabberNotifyEvent read FOnRequestSent write FOnRequestSent;
    property OnRequestComplete: TJvGrabberNotifyEvent read FOnRequestComplete write FOnRequestComplete;
    property OnReceivingResponse: TJvGrabberNotifyEvent read FOnReceivingResponse write FOnReceivingResponse;
    property OnResponseReceived: TJvGrabberNotifyEvent read FOnResponseReceived write FOnResponseReceived;
    property OnClosingConnection: TJvGrabberNotifyEvent read FOnClosingConnection write FOnClosingConnection;
    property OnConnectionClosed: TJvGrabberNotifyEvent read FOnConnectionClosed write FOnConnectionClosed;
    property OnRedirect: TJvGrabberNotifyEvent read FOnRedirect write FOnRedirect;
    property OnStatusChange: TJvGrabberNotifyEvent read FOnStatusChange write FOnStatusChange;

    // Events for component
    property OnGrabberCreated: TJvGrabberCreatedEvent read FOnGrabberCreated write FOnGrabberCreated;
    property OnGrabberAdded: TJvGrabberAddedEvent read FOnGrabberAdded write FOnGrabberAdded;
  end;

  // forward declarations
  TJvCustomUrlGrabberThread = class;
  TJvCustomUrlGrabberThreadClass = class of TJvCustomUrlGrabberThread;
  TJvCustomUrlGrabberDefaultProperties = class;

  // a trick for the Delphi editor that allows to have a sub object
  // for each member of the a TJvUrlGrabberDefaultPropertiesList
  // Because an indexed property cannot be published, the editor
  // for TJvUrlGrabberDefaultPropertiesList enumerates all the
  // items in the list, and passes the EditorTrick property of
  // each of its TJvUrlGrabberDefaultProperties members. The
  // trick contains only one published property that gets displayed
  // and this property points to the TJvUrlGrabberDefaultPropertiesList
  // object to which the trick belongs, thus allowing to publish
  // the indexed property. The only drawback is that the name
  // in the property editor for each object is the same, DefaultProperties
  // Hence, the need for an editor for TJvUrlGrabberDefPropEdTrick that
  // displays a meaningful name instead
  TJvUrlGrabberDefPropEdTrick = class(TPersistent)
  private
    FDefaultProperties: TJvCustomUrlGrabberDefaultProperties;
  public
    constructor Create(GrabberDefaults: TJvCustomUrlGrabberDefaultProperties); reintroduce; virtual;
  published
    property DefaultProperties: TJvCustomUrlGrabberDefaultProperties read FDefaultProperties;
  end;

  // A container for Default properties, and a list of such
  // containers
  TJvCustomUrlGrabberDefaultProperties = class(TPersistent)
  private
    FEditorTrick: TJvUrlGrabberDefPropEdTrick;
    // agent to impersonate
    FAgent: string;
    // Port to connect to
    FPort: Cardinal;
    // user information
    FUserName: string;
    FPassword: string;
    // filename to use
    FFileName: TFileName;
    // output mode (stream or file)
    FOutputMode: TJvOutputMode;
  protected
    // The user-friendly name of the supported URL type
    function GetSupportedURLName: string; virtual; abstract;
    // The agent to impersonate
    property Agent: string read FAgent write FAgent;
    // The port to connect to
    property Port: Cardinal read FPort write FPort;
    // the user name and password to use for authentication
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
  public
    constructor Create(AOwner: TJvUrlGrabberDefaultPropertiesList); reintroduce; virtual;
    destructor Destroy; override;
    // for some odd reason, Assign needs to be overriden
    procedure Assign(Source: TPersistent); override;
    property EditorTrick: TJvUrlGrabberDefPropEdTrick read FEditorTrick;
    property SupportedURLName: string read GetSupportedURLName;
  published
    // the name of the file to write to if OutputMode is omFile
    property FileName: TFileName read FFileName write FFileName;
    // The output mode
    property OutputMode: TJvOutputMode read FOutputMode write FOutputMode default omStream;
  end;

  TJvCustomUrlGrabberDefaultPropertiesClass = class of TJvCustomUrlGrabberDefaultProperties;

  TJvUrlGrabberDefaultPropertiesList = class(TPersistent)
  private
    FItems: TObjectList;
    function GetItemsNamed(Name: string): TJvCustomUrlGrabberDefaultProperties;
    function GetCount: Integer;
    function GetItems(Index: Integer): TJvCustomUrlGrabberDefaultProperties;
    procedure SetItems(Index: Integer; const Value: TJvCustomUrlGrabberDefaultProperties);
  public
    constructor Create(AOwner: TJvUrlListGrabber); reintroduce; virtual;
    destructor Destroy; override;
    procedure Read(Reader: TReader);
    procedure Write(Writer: TWriter);
    procedure Clear;
    procedure Add(Item: TJvCustomUrlGrabberDefaultProperties);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJvCustomUrlGrabberDefaultProperties read GetItems write SetItems;
    property ItemsNamed[Name: string]: TJvCustomUrlGrabberDefaultProperties read GetItemsNamed; default;
  end;

  // the status of a grabber
  TJvGrabberStatus = (gsStopped, gsConnecting, gsGrabbing, gsStopping);

  // The exception triggered if someone tries to set the Url property while the
  // grabber is not stopped
  EGrabberNotStopped = class(Exception);

  // The event type used when a grabbing has had some progress
  TJvUrlGrabberProgressEvent = procedure(Sender: TObject; Position, TotalSize: Int64;
    Url: string; var Continue: Boolean) of object;

  // The ancestor of all the Url Grabbers that declares the required
  // methods that a grabber must provide.
  // Do not instanciate a TJvCustomUrlGrabber directly, simply use one
  // of its descendants. This family of classes is used by
  // TJvUrlListGrabber to allow downloading a list of URLs but can
  // also be used on their own to grab one URL of a given type.
  TJvCustomUrlGrabber = class(TJvComponent)
  private
    FId: Integer;
    // the thread that will grab for us
    FUrlGrabberThread: TJvCustomUrlGrabberThread;
    // events
    FOnDoneFile: TJvDoneFileEvent; // file is done
    FOnDoneStream: TJvDoneStreamEvent; // stream is done
    FOnError: TJvErrorEvent; // error occurred
    FOnProgress: TJvUrlGrabberProgressEvent; // download progressed a bit
    FOnClosed: TNotifyEvent; // connection is closed
    FOnReceiving: TNotifyEvent; // beginning to receive
    FOnReceived: TNotifyEvent; // end of reception
    FOnConnecting: TNotifyEvent; // beginning of connection
    FOnResolving: TNotifyEvent; // beginning of resolving URL
    FOnRedirect: TNotifyEvent; // redirection happened
    FOnConnected: TNotifyEvent; // now connected to host
    //FOnStateChange: TNotifyEvent; // state of connection changed
    FOnResolved: TNotifyEvent; // name has been resolved
    FOnClosing: TNotifyEvent; // beginning of close of connection
    FOnRequest: TNotifyEvent; // sending a request
    FOnSent: TNotifyEvent; // data sent
    FOnSending: TNotifyEvent; // beginning to send data
    FOnStatusChange: TNotifyEvent; // Status changed
    // current status of the grabber
    FStatus: TJvGrabberStatus;
    // URL to grab
    FUrl: string;
    // the stream to grab into.
    FStream: TMemoryStream;
    // agent to impersonate
    FAgent: string;
    // port to connect to
    FPort: Cardinal;
    // user information
    FUserName: string;
    FPassword: string;
    // filename to use
    FFileName: TFileName;
    // output mode (stream or file)
    FOutputMode: TJvOutputMode;
    // size of the file to grab
    FSize: Int64;
    // What has been read so far
    FBytesRead: Int64;
    // True if data has been read
    FGrabbingStarted: Boolean;

    FTimeOut: TJvUrlGrabberTimeOut;
  protected
    // Event callers
    procedure DoError(ErrorMsg: string);
    procedure DoProgress(Position: Integer; var Continue: Boolean);
    procedure DoStatus; virtual;
    procedure DoEnded;
    procedure DoClosed;
    procedure SetSize(Value: Int64);
    procedure SetBytesRead(Value: Int64);
    function GetGrabberThreadClass: TJvCustomUrlGrabberThreadClass; virtual; abstract;
    procedure SetUrl(Value: string); virtual;
    property UrlGrabberThread: TJvCustomUrlGrabberThread read FUrlGrabberThread;
    property Stream: TMemoryStream read FStream write FStream;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AUrl: string;
      DefaultProperties: TJvCustomUrlGrabberDefaultProperties); reintroduce; overload;
    destructor Destroy; override;
    // this function must return True if the given URL can be grabbed
    // by the class being asked. It returns False otherwise
    // It MUST be overriden in the derived classes but cannot be abstract
    // because of BCB compatibility issues (no support for abstract
    // - pure virtual - class functions in the C++ language)
    class function CanGrab(const Url: string): Boolean; virtual;
    // This function returns the class of a property holder to
    // be displayed in the object inspector. This property holder
    // will be used by TJvUrlListGrabber to let the user specify default
    // properties and will be passed to this class when created to
    // handle a specific URL.
    // It MUST be overriden in the derived classes but cannot be abstract
    // because of BCB compatibility issues (no support for abstract
    // - pure virtual - class functions in the C++ language)
    class function GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass; virtual;
    // This function returns the marker that indicates the protocol in a URL.
    // For instance, for an HTTP grabber, this would return 'http://'
    // It MUST be overriden in the derived classes but cannot be abstract
    // because of BCB compatibility issues (no support for abstract
    // - pure virtual - class functions in the C++ language)
    class function GetSupportedProtocolMarker: string; virtual;
    // this function must return a user displayable string indicating
    // the type of URL that class of grabber supports.
    // It MUST be overriden in the derived classes but cannot be abstract
    // because of BCB compatibility issues (no support for abstract
    // - pure virtual - class functions in the C++ language)
    class function GetSupportedURLName: string; virtual;
    // Splits the given URL into its various parts, if indicated
    // A URL respects this format:
    // protocol [username[:password]@] host [:port] [/filename]
    // When a non compulsory part is missing the exit value of the
    // associated parameter will be an empty string or 0
    class procedure ParseUrl(URL: string; Protocol: string; var Host: string; var FileName: string;
      var UserName: string; var Password: string; var Port: Cardinal); virtual;

    class function GetFormattedUrl(const URL: string): string;

    // Asks to Start to grab the URL
    procedure Start; virtual;
    // Asks to Stop to grab the URL
    procedure Stop; virtual;
    // The status of the grab
    property Status: TJvGrabberStatus read FStatus;
    // The size of the file being grabbed
    property Size: Int64 read FSize;
    // What has been read so far
    property BytesRead: Int64 read FBytesRead;
    // the Url being grabbed
    property Url: string read FUrl write SetUrl;
    // The port to connect to
    property Port: Cardinal read FPort write FPort;
    // the user name and password to use for authentication
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    // the name of the file to write to if OutputMode is omFile
    property FileName: TFileName read FFileName write FFileName;
    // The output mode
    property OutputMode: TJvOutputMode read FOutputMode write FOutputMode default omFile;
    // The agent to impersonate
    property Agent: string read FAgent write FAgent;
    // A numerical Id, to be freely used by the user of the component
    property Id: Integer read FId write FId;
    // Timeout values
    property TimeOut: TJvUrlGrabberTimeOut read FTimeOut;
    // Events
    property OnDoneFile: TJvDoneFileEvent read FOnDoneFile write FOnDoneFile;
    property OnDoneStream: TJvDoneStreamEvent read FOnDoneStream write FOnDoneStream;
    property OnError: TJvErrorEvent read FOnError write FOnError;
    property OnProgress: TJvUrlGrabberProgressEvent read FOnProgress write FOnProgress;
    property OnResolvingName: TNotifyEvent read FOnResolving write FOnResolving;
    property OnNameResolved: TNotifyEvent read FOnResolved write FOnResolved;
    property OnConnectingToServer: TNotifyEvent read FOnConnecting write FOnConnecting;
    property OnConnectedToServer: TNotifyEvent read FOnConnected write FOnConnected;
    property OnSendingRequest: TNotifyEvent read FOnSending write FOnSending;
    property OnRequestSent: TNotifyEvent read FOnSent write FOnSent;
    property OnRequestComplete: TNotifyEvent read FOnRequest write FOnRequest;
    property OnReceivingResponse: TNotifyEvent read FOnReceiving write FOnReceiving;
    property OnResponseReceived: TNotifyEvent read FOnReceived write FOnReceived;
    property OnClosingConnection: TNotifyEvent read FOnClosing write FOnClosing;
    property OnConnectionClosed: TNotifyEvent read FOnClosed write FOnClosed;
    property OnRedirect: TNotifyEvent read FOnRedirect write FOnRedirect;
    property OnStatusChange: TNotifyEvent read FOnStatusChange write FOnStatusChange;
  end;

  // A thread that will grab the given URL in the background
  // this is the ancestor of all the grabber threads, and there
  // should be as many descendants as there are TJvCustomUrlGrabber
  // descendants.
  TJvCustomUrlGrabberThread = class(TJvCustomThread)
  private
    FErrorText: string; // the error string received from the server
    FAPIStatus: DWORD;
    FContinue: Boolean;
  protected
    FGrabber: TJvCustomUrlGrabber;

    procedure Execute; override;

    // Derived classes must not override Execute. They must instead override
    // Grab which is called by this class' Execute. This is done to ensure
    // that all derived classes will always set the status back to gsStopped
    // and trigger the OnConnectionClosed event at the end.
    procedure Grab; virtual; abstract;

    procedure Error;
    procedure Ended;
    procedure Closed;

    procedure UpdateGrabberProgress;
    procedure UpdateGrabberStatus;

    // Procedure used by derived classes to set the value of FStatus
    // which is a private member of TJvCustomUrlGrabber
    procedure SetGrabberStatus(Status: TJvGrabberStatus);

    property ErrorText: string read FErrorText write FErrorText;
    property Continue: Boolean read FContinue write FContinue;
  public
    constructor Create(Grabber: TJvCustomUrlGrabber); virtual;
    procedure DoProgress;
    procedure DoStatus;
    property APIStatus: DWORD read FAPIStatus write FAPIStatus;
  end;

  // A list of instances of TJvUrlGrabber descendants
  // This is used internally by TJvUrlListGrabber to keep track of
  // the objects in charge of every URLs it has to grab
  TJvUrlGrabberList = class(TObjectList)
  private
    function GetItem(Index: Integer): TJvCustomUrlGrabber;
    procedure SetItem(Index: Integer; const AGrabber: TJvCustomUrlGrabber);
  public
    function Add(AGrabber: TJvCustomUrlGrabber): Integer;
    procedure Insert(Index: Integer; AGrabber: TJvCustomUrlGrabber);
    property Items[Index: Integer]: TJvCustomUrlGrabber read GetItem write SetItem; default;
  end;

  TJvCustomUrlGrabberClass = class of TJvCustomUrlGrabber;

  // A list of classes inheriting from TJvCustomUrlGrabber
  // This is the type of list used by the JvUrlGrabberClassList
  // function that returns all the registered classes.
  // This list is then used by TJvUrlListGrabber to determine which
  // class is best suited for handling a given URL
  TJvUrlGrabberClassList = class(TClassList)
  private
    function GetItem(Index: Integer): TJvCustomUrlGrabberClass;
    procedure SetItem(Index: Integer; const AGrabberClass: TJvCustomUrlGrabberClass);
  public
    procedure Populate(DefaultPropertiesList: TJvUrlGrabberDefaultPropertiesList);
    function Add(AGrabberClass: TJvCustomUrlGrabberClass): Integer;
    procedure Insert(Index: Integer; AGrabberClass: TJvCustomUrlGrabberClass);
    function CreateFor(Owner: TComponent; Url: string; DefaultPropertiesList: TJvUrlGrabberDefaultPropertiesList):
      TJvCustomUrlGrabber;
    property Items[Index: Integer]: TJvCustomUrlGrabberClass read GetItem write SetItem; default;
  end;

function JvUrlGrabberClassList: TJvUrlGrabberClassList;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvResources,
  // JvUrlGrabbers MUST be included here so that the grabbers
  // it contains are registered before any JvUrlListGrabber
  // component reads its properties.
  JvUrlGrabbers;

var
  // the global object to contain the list of registered
  // url grabber classes
  GJvUrlGrabberClassList: TJvUrlGrabberClassList = nil;

function JvUrlGrabberClassList: TJvUrlGrabberClassList;
begin
  if not Assigned(GJvUrlGrabberClassList) then
    GJvUrlGrabberClassList := TJvUrlGrabberClassList.Create;
  Result := GJvUrlGrabberClassList;
end;

//=== { TJvUrlListGrabber } ==================================================

constructor TJvUrlListGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCleanupThreshold := 10;
  FCleanupList := TObjectList.Create(True);
  FDefaultGrabbersProperties := TJvUrlGrabberDefaultPropertiesList.Create(Self);
  FGrabbers := TJvUrlGrabberList.Create(True);
  FURLs := TStringList.Create;
  FURLs.OnChanging := URLsChanging;
  FDefaultGrabberIndex := -1;

  FTimeOut := TJvUrlGrabberTimeOut.Create;
end;

destructor TJvUrlListGrabber.Destroy;
begin
  FURLs.Free;
  FGrabbers.Free;
  FDefaultGrabbersProperties.Free;
  FCleanupList.Free;
  FTimeOut.Free;

  inherited Destroy;
end;

procedure TJvUrlListGrabber.Cleanup;
{var
  I: Integer;}
begin
{  // try to find each created grabber in the string list
  // if not found, mark the object as nil which in turn
  // will delete it
  for I := 0 to FGrabbers.Count - 1 do
    if FURLs.IndexOfObject(FGrabbers[I]) = -1 then
      FGrabbers[I] := nil;
  // pack the list
  FGrabbers.Pack;}
  FCleanupList.Clear;
end;

function TJvUrlListGrabber.GetGrabbers(const Index: Integer): TJvCustomUrlGrabber;
begin
  Result := FGrabbers[Index];
end;

procedure TJvUrlListGrabber.SetDefaultGrabberIndex(const Value: TJvUrlGrabberIndex);
begin
  if Value < -1 then
    FDefaultGrabberIndex := -1
  else
  if Value > JvUrlGrabberClassList.Count - 1 then
    FDefaultGrabberIndex := -1
  else
    FDefaultGrabberIndex := Value;
end;

function TJvUrlListGrabber.GetURLs: TStrings;
begin
  Result := FURLs;
end;

procedure TJvUrlListGrabber.SetURLs(const Value: TStrings);
begin
  FURLs.Assign(Value);
end;

procedure TJvUrlListGrabber.DoGrabberCreated(Grabber: TJvCustomUrlGrabber);
begin
  if Assigned(OnGrabberCreated) then
    OnGrabberCreated(Self, Grabber);
end;

procedure TJvUrlListGrabber.DoGrabberAdded(Grabber: TJvCustomUrlGrabber; Index: Integer);
begin
  if Assigned(OnGrabberAdded) then
    OnGrabberAdded(Self, Grabber, Index);
end;

function TJvUrlListGrabber.GetGrabberForUrl(const URL: string): TJvCustomUrlGrabber;
begin
  Result := JvUrlGrabberClassList.CreateFor(Self, URL, FDefaultGrabbersProperties);
  if not Assigned(Result) then
    if DefaultGrabberIndex > -1 then
      Result := JvUrlGrabberClassList[DefaultGrabberIndex].Create(Self, URL,
        FDefaultGrabbersProperties.Items[DefaultGrabberIndex])
    else
      raise ENoGrabberForUrl.CreateResFmt(@RsENoGrabberForUrl, [URL]);

  DoGrabberCreated(Result);
end;

procedure TJvUrlListGrabber.StartNextGrabber;
var
  NewGrabber: TJvCustomUrlGrabber;
begin
  NewGrabber := GetGrabberForUrl(FURLs[FNextUrlIndex]);
  Inc(FNextUrlIndex);  // Inc everytime to be thread safe

  NewGrabber.TimeOut.Assign(FTimeOut);
  SetGrabberEvents(NewGrabber);
  DoGrabberAdded(NewGrabber, FGrabbers.Add(NewGrabber));
  NewGrabber.Start;
end;

procedure TJvUrlListGrabber.Start;
var
  I: Integer;
  MaxNewGrabbers: Integer;
begin
  // If at least one grabber is running, then do not start
  if GrabberCount > 0 then
    Exit;

  FGrabbers.Clear;

  if (MaxSimultaneousGrabbers = 0) or (FURLs.Count < MaxSimultaneousGrabbers) then
    MaxNewGrabbers := FURLs.Count
  else
    MaxNewGrabbers := MaxSimultaneousGrabbers;


  FNextUrlIndex := 0;
  for I := 0 to MaxNewGrabbers - 1 do
    StartNextGrabber;
end;

{$WARNINGS OFF} // hide deprecated warning when compiling the JVCL
procedure TJvUrlListGrabber.StartAll;
{$WARNINGS ON}
begin
  Start;
end;

procedure TJvUrlListGrabber.Stop;
var
  I: Integer;
begin
  FNextUrlIndex := FURLs.Count; // Prevent from creating new grabbers
  for I := 0 to GrabberCount - 1 do
    Grabbers[I].Stop;
end;

{$WARNINGS OFF} // hide deprecated warning when compiling the JVCL
procedure TJvUrlListGrabber.StopAll;
{$WARNINGS ON}
begin
  Stop;
end;

procedure TJvUrlListGrabber.URLsChanging(Sender: TObject);
begin
  // Prevent changing the URLs while at least one grabber is running
  if GrabberCount > 0 then
    raise EAtLeastOneGrabberRunning.CreateRes(@RsEAtLeastOneGrabberRunning);
end;

function TJvUrlListGrabber.GetGrabberCount: Integer;
begin
  Result := FGrabbers.Count;
end;

procedure TJvUrlListGrabber.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('DefaultGrabbersPropertiesList',
    DefaultGrabbersProperties.Read, DefaultGrabbersProperties.Write, True);
end;

procedure TJvUrlListGrabber.SetGrabberEvents(Grabber: TJvCustomUrlGrabber);
begin
  Grabber.OnClosingConnection := GrabberClosingConnection;
  Grabber.OnConnectedToServer := GrabberConnectedToServer;
  Grabber.OnConnectingToServer := GrabberConnectingToServer;
  Grabber.OnConnectionClosed := GrabberConnectionClosed;
  Grabber.OnNameResolved := GrabberNameResolved;
  Grabber.OnReceivingResponse := GrabberReceivingResponse;
  Grabber.OnRedirect := GrabberRedirect;
  Grabber.OnRequestComplete := GrabberRequestComplete;
  Grabber.OnRequestSent := GrabberRequestSent;
  Grabber.OnResolvingName := GrabberResolvingName;
  Grabber.OnResponseReceived := GrabberResponseReceived;
  Grabber.OnSendingRequest := GrabberSendingRequest;
  Grabber.OnStatusChange := GrabberStatusChange;
  Grabber.OnError := GrabberError;
  Grabber.OnProgress := GrabberProgress;
  Grabber.OnDoneFile := GrabberDoneFile;
  Grabber.OnDoneStream := GrabberDoneStream;
end;

procedure TJvUrlListGrabber.GrabberClosingConnection(Grabber: TObject);
begin
  if Assigned(OnClosingConnection) then
    OnClosingConnection(Self, TJvCustomUrlGrabber(Grabber));
end;

procedure TJvUrlListGrabber.GrabberConnectedToServer(Grabber: TObject);
begin
  if Assigned(OnConnectedToServer) then
    OnConnectedToServer(Self, TJvCustomUrlGrabber(Grabber));
end;

procedure TJvUrlListGrabber.GrabberConnectingToServer(Grabber: TObject);
begin
  if Assigned(OnConnectingToServer) then
    OnConnectingToServer(Self, TJvCustomUrlGrabber(Grabber));
end;

procedure TJvUrlListGrabber.GrabberConnectionClosed(Grabber: TObject);
begin
  // Grabber has closed connection, meaning that it has finished, hence we check
  // the cleanup threshold, move the finished grabber to the cleanup list and
  // start a new grabber if there are URLs left
  if Cardinal(FCleanupList.Count) = CleanupThreshold then
    FCleanupList.Clear;

  FCleanupList.Add(FGrabbers.Extract(Grabber));

  if FNextUrlIndex < FURLs.Count then
    StartNextGrabber;

  if Assigned(OnConnectionClosed) then
    OnConnectionClosed(Self, TJvCustomUrlGrabber(Grabber));
end;

procedure TJvUrlListGrabber.GrabberNameResolved(Grabber: TObject);
begin
  if Assigned(OnNameResolved) then
    OnNameResolved(Self, TJvCustomUrlGrabber(Grabber));
end;

procedure TJvUrlListGrabber.GrabberReceivingResponse(Grabber: TObject);
begin
  if Assigned(OnReceivingResponse) then
    OnReceivingResponse(Self, TJvCustomUrlGrabber(Grabber));
end;

procedure TJvUrlListGrabber.GrabberRedirect(Grabber: TObject);
begin
  if Assigned(OnRedirect) then
    OnRedirect(Self, TJvCustomUrlGrabber(Grabber));
end;

procedure TJvUrlListGrabber.GrabberRequestComplete(Grabber: TObject);
begin
  if Assigned(OnRequestComplete) then
    OnRequestComplete(Self, TJvCustomUrlGrabber(Grabber));
end;

procedure TJvUrlListGrabber.GrabberRequestSent(Grabber: TObject);
begin
  if Assigned(OnRequestSent) then
    OnRequestSent(Self, TJvCustomUrlGrabber(Grabber));
end;

procedure TJvUrlListGrabber.GrabberResolvingName(Grabber: TObject);
begin
  if Assigned(OnResolvingName) then
    OnResolvingName(Self, TJvCustomUrlGrabber(Grabber));
end;

procedure TJvUrlListGrabber.GrabberResponseReceived(Grabber: TObject);
begin
  if Assigned(OnResponseReceived) then
    OnResponseReceived(Self, TJvCustomUrlGrabber(Grabber));
end;

procedure TJvUrlListGrabber.GrabberSendingRequest(Grabber: TObject);
begin
  if Assigned(OnSendingRequest) then
    OnSendingRequest(Self, TJvCustomUrlGrabber(Grabber));
end;

procedure TJvUrlListGrabber.GrabberStatusChange(Grabber: TObject);
begin
  if Assigned(OnStatusChange) then
    OnStatusChange(Self, TJvCustomUrlGrabber(Grabber));
end;

procedure TJvUrlListGrabber.GrabberDoneFile(Grabber: TObject; FileName: string;
  FileSize: Integer; Url: string);
begin
  if Assigned(OnDoneFile) then
    OnDoneFile(Self, TJvCustomUrlGrabber(Grabber), FileName, FileSize, Url);
end;

procedure TJvUrlListGrabber.GrabberDoneStream(Grabber: TObject; Stream: TStream;
  StreamSize: Integer; Url: string);
begin
  if Assigned(OnDoneStream) then
    OnDoneStream(Self, TJvCustomUrlGrabber(Grabber), Stream, StreamSize, Url);
end;

procedure TJvUrlListGrabber.GrabberError(Grabber: TObject; ErrorMsg: string);
begin
  if Assigned(OnError) then
    OnError(Self, TJvCustomUrlGrabber(Grabber), ErrorMsg);
end;

procedure TJvUrlListGrabber.GrabberProgress(Grabber: TObject; Position, TotalSize: Int64;
  Url: string; var Continue: Boolean);
begin
  if Assigned(OnProgress) then
    OnProgress(Self, TJvCustomUrlGrabber(Grabber), Position, TotalSize, Url, Continue);
end;

//=== { TJvCustomUrlGrabber } ================================================

constructor TJvCustomUrlGrabber.Create(AOwner: TComponent; AUrl: string;
  DefaultProperties: TJvCustomUrlGrabberDefaultProperties);
begin
  Create(AOwner);

  FUrlGrabberThread := nil;

  // get values from the default properties
  Agent := DefaultProperties.Agent;
  UserName := DefaultProperties.UserName;
  Password := DefaultProperties.Password;
  FileName := DefaultProperties.FileName;
  OutputMode := DefaultProperties.OutputMode;
  Port := DefaultProperties.Port;

  // Set the URL at the end so that the SetUrl method is called
  // and might setup the various other properties automatically
  Url := AUrl;
end;

constructor TJvCustomUrlGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTimeOut := TJvUrlGrabberTimeOut.Create;

  // Set default properties
  Agent := RsJediAgent;
  Port := 0;
  UserName := '';
  Password := '';
  FileName := RsDefaultOutputFileName;
  OutputMode := omFile;
end;

destructor TJvCustomUrlGrabber.Destroy;
begin
  Stop;  // Stop grabbing

  FTimeOut.Free;

  inherited Destroy;
end;

class function TJvCustomUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  // useless implementation required for BCB compatibility as
  // C++ doesn't support abstract virtual class methods
  Result := False;
end;

procedure TJvCustomUrlGrabber.SetUrl(Value: string);
var
  ProtocolMarker: string;
  TmpHostName: string;
  TmpFileName: string;
  TmpUserName: string;
  TmpPassword: string;
  TmpPort: Cardinal;
begin
  if Status = gsStopped then
  begin
    // if the given URL contains Port, UserName and Password informations, we set the
    // different properties of the grabber automatically
    ProtocolMarker := GetSupportedProtocolMarker;
    ParseUrl(Value, ProtocolMarker, TmpHostName, TmpFileName, TmpUserName, TmpPassword, TmpPort);
    if TmpUserName <> '' then
      UserName := TmpUserName;
    if TmpPassword <> '' then
      Password := TmpPassword;
    if TmpPort <> 0 then
      Port := TmpPort;

    FUrl := GetFormattedUrl(Value);
  end
  else
    raise EGrabberNotStopped.CreateRes(@RsEGrabberNotStopped);
end;

procedure TJvCustomUrlGrabber.DoClosed;
begin
  if Assigned(FOnClosed) then
    FOnClosed(Self);
end;

procedure TJvCustomUrlGrabber.DoEnded;
begin
  // Don't create empty file/stream if we didn't start grabbing
  if not FGrabbingStarted Then
    Exit;

  Stream.Position := 0;
  if FOutputMode = omStream then
  begin
    if Assigned(FOnDoneStream) then
      FOnDoneStream(Self, Stream, Stream.Size, FUrl);
  end
  else
  begin
    Stream.SaveToFile(FFileName);
    if Assigned(FOnDoneFile) then
      FOnDoneFile(Self, FFileName, Stream.Size, FUrl);
  end;
end;

procedure TJvCustomUrlGrabber.DoError(ErrorMsg: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, ErrorMsg);
end;

procedure TJvCustomUrlGrabber.DoProgress(Position: Integer; var Continue: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Position, FSize, Url, Continue);
end;

procedure TJvCustomUrlGrabber.DoStatus;
begin
  if Assigned(FOnStatusChange) then
    FOnStatusChange(Self);
end;

class function TJvCustomUrlGrabber.GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass;
begin
  // useless implementation for BCB Compatibility
  Result := nil;
end;

procedure TJvCustomUrlGrabber.Start;
begin
  // Stop the grabbing before restarting
  Stop;

  // Create a new thread
  FUrlGrabberThread := GetGrabberThreadClass.Create(Self);
  FUrlGrabberThread.{$IFDEF COMPILER14_UP}Start{$ELSE}Resume{$ENDIF COMPILER14_UP};
end;

procedure TJvCustomUrlGrabber.Stop;
begin
  if Assigned(FUrlGrabberThread) then
  begin
    // If there is a thread, ask it to terminate and then free it.
    // This will ensure that everything is cleanly destroyed (Mantis 3824).
    FUrlGrabberThread.Terminate;
    FUrlGrabberThread.Free;
    FUrlGrabberThread := nil;  // To avoid crashing if calling "Stop" twice or more.
  end;
end;

procedure TJvCustomUrlGrabber.SetSize(Value: Int64);
begin
  FSize := Value;
end;

procedure TJvCustomUrlGrabber.SetBytesRead(Value: Int64);
begin
  FBytesRead := Value;
end;

class function TJvCustomUrlGrabber.GetSupportedProtocolMarker: string;
begin
  // Useless implementation for BCB compatibility
  Result := '';
end;

class function TJvCustomUrlGrabber.GetSupportedURLName: string;
begin
  // Useless implementation for BCB compatibility
  Result := '';
end;

class function TJvCustomUrlGrabber.GetFormattedUrl(const URL: string): string;
var
  ProtocolMarker: string;
  TmpHostName: string;
  TmpFileName: string;
  TmpUserName: string;
  TmpPassword: string;
  TmpPort: Cardinal;
begin
  ProtocolMarker := GetSupportedProtocolMarker;
  ParseUrl(URL, ProtocolMarker, TmpHostName, TmpFileName, TmpUserName, TmpPassword, TmpPort);

  Result := ProtocolMarker;
  if TmpHostName <> '' then
    Result := Result + TmpHostName + '/';
  if TmpFileName <> '' then
    Result := Result + TmpFileName;
end;

class procedure TJvCustomUrlGrabber.ParseUrl(URL: string; Protocol: string;
  var Host: string; var FileName: string; var UserName: string;
  var Password: string; var Port: Cardinal);
var
  Ps: Integer;
begin
  // Default return values
  Host := '';
  FileName := '';
  UserName := '';
  Password := '';
  Port := 0;

  // Remove the protocol part from the given Value
  if Pos(UpperCase(Protocol), UpperCase(URL)) <> 0 then
    URL := Copy(URL, Length(Protocol) + 1, Length(URL));

  // Get the filename, if any
  if Pos('/', URL) <> 0 then
  begin
    Ps := Pos('/', URL);
    Host := Copy(URL, 1, Ps - 1);
    FileName := Copy(URL, Ps + 1, Length(URL));
  end
  else
    Host := URL;

  // Get the username password couple
  Ps := Pos('@', Host);
  if Ps <> 0 then
  begin
    UserName := Copy(Host, 1, Ps - 1);
    Host := Copy(Host, Ps + 1, Length(Host));
    // now, figure out if there is a password
    Ps := Pos(':', UserName);
    if Ps <> 0 then
    begin
      Password := Copy(UserName, Ps + 1, Length(UserName));
      UserName := Copy(UserName, 1, Ps - 1);
    end;
  end;

  // Get the port
  Ps := Pos(':', Host);
  if Ps <> 0 then
  begin
    Port := StrToIntDef(Copy(Host, Ps + 1, Length(Host)), 0);
    Host := Copy(Host, 1, Ps - 1);
  end;
end;

//=== { TJvUrlGrabberList } ==================================================

function TJvUrlGrabberList.Add(AGrabber: TJvCustomUrlGrabber): Integer;
begin
  Result := inherited Add(AGrabber);
end;

function TJvUrlGrabberList.GetItem(Index: Integer): TJvCustomUrlGrabber;
begin
  Result := TJvCustomUrlGrabber(inherited Items[Index]);
end;

procedure TJvUrlGrabberList.Insert(Index: Integer; AGrabber: TJvCustomUrlGrabber);
begin
  inherited Insert(Index, AGrabber);
end;

procedure TJvUrlGrabberList.SetItem(Index: Integer; const AGrabber: TJvCustomUrlGrabber);
begin
  inherited Items[Index] := AGrabber;
end;

//=== { TJvCustomUrlGrabberClassList } =======================================

function TJvUrlGrabberClassList.Add(AGrabberClass: TJvCustomUrlGrabberClass): Integer;
begin
  Result := inherited Add(AGrabberClass);
end;

function TJvUrlGrabberClassList.CreateFor(Owner: TComponent; Url: string;
  DefaultPropertiesList: TJvUrlGrabberDefaultPropertiesList): TJvCustomUrlGrabber;
var
  I: Integer;
begin
  I := 0;
  Result := nil;
  while (I < Count) and not Assigned(Result) do
  begin
    if Items[I].CanGrab(Url) then
      Result := Items[I].Create(Owner, Url, DefaultPropertiesList.Items[I]);
    Inc(I);
  end;
end;

function TJvUrlGrabberClassList.GetItem(Index: Integer): TJvCustomUrlGrabberClass;
begin
  Result := TJvCustomUrlGrabberClass(inherited Items[Index]);
end;

procedure TJvUrlGrabberClassList.Insert(Index: Integer;
  AGrabberClass: TJvCustomUrlGrabberClass);
begin
  inherited Insert(Index, AGrabberClass);
end;

procedure TJvUrlGrabberClassList.Populate(DefaultPropertiesList: TJvUrlGrabberDefaultPropertiesList);
var
  I: Integer;
begin
  DefaultPropertiesList.Clear;
  for I := 0 to Count - 1 do
    DefaultPropertiesList.Add(Items[I].GetDefaultPropertiesClass.Create(DefaultPropertiesList));
end;

procedure TJvUrlGrabberClassList.SetItem(Index: Integer;
  const AGrabberClass: TJvCustomUrlGrabberClass);
begin
  inherited Items[Index] := AGrabberClass;
end;

//=== { TJvCustomUrlGrabberThread } ==========================================

procedure TJvCustomUrlGrabberThread.Closed;
begin
  FGrabber.DoClosed;
end;

constructor TJvCustomUrlGrabberThread.Create(Grabber: TJvCustomUrlGrabber);
begin
  inherited Create(True);
  FContinue := True;
  FGrabber := Grabber;
  ThreadName := Format('%s: %s',[ClassName, Grabber.Name]);
end;

procedure TJvCustomUrlGrabberThread.DoProgress;
begin
  Synchronize(UpdateGrabberProgress);
end;

procedure TJvCustomUrlGrabberThread.DoStatus;
begin
  Synchronize(UpdateGrabberStatus);
end;

procedure TJvCustomUrlGrabberThread.Ended;
begin
  FGrabber.DoEnded;
end;

procedure TJvCustomUrlGrabberThread.Error;
begin
  FGrabber.FStatus := gsStopped;
  FGrabber.DoError(FErrorText);
end;

procedure TJvCustomUrlGrabberThread.Execute;
begin
  NameThread(ThreadName);
  SetGrabberStatus(gsStopped);
  FGrabber.FGrabbingStarted := False;
  FGrabber.Stream := nil;
  try
    Grab;
  finally
    //Free all stuff's
    FGrabber.Stream.Free;
    FGrabber.Stream := nil;

    // Signal Closed, after having changed the state of the grabber
    SetGrabberStatus(gsStopped);
    Synchronize(Closed);
  end;
end;

procedure TJvCustomUrlGrabberThread.SetGrabberStatus(Status: TJvGrabberStatus);
begin
  FGrabber.FStatus := Status;
  if Status = gsGrabbing then
    FGrabber.FGrabbingStarted := True;
end;

procedure TJvCustomUrlGrabberThread.UpdateGrabberProgress;
begin
  FGrabber.DoProgress(FGrabber.BytesRead, FContinue);
end;

procedure TJvCustomUrlGrabberThread.UpdateGrabberStatus;
begin
  FGrabber.DoStatus;
end;

//=== { TJvUrlGrabberDefaultPropertiesList } =================================

constructor TJvUrlGrabberDefaultPropertiesList.Create(AOwner: TJvUrlListGrabber);
begin
  inherited Create;
  FItems := TObjectList.Create(True);
  JvUrlGrabberClassList.Populate(Self);
end;

destructor TJvUrlGrabberDefaultPropertiesList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TJvUrlGrabberDefaultPropertiesList.Add(Item: TJvCustomUrlGrabberDefaultProperties);
begin
  FItems.Add(Item);
end;

procedure TJvUrlGrabberDefaultPropertiesList.Clear;
begin
  FItems.Clear;
end;

function TJvUrlGrabberDefaultPropertiesList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvUrlGrabberDefaultPropertiesList.GetItems(Index: Integer): TJvCustomUrlGrabberDefaultProperties;
begin
  Result := TJvCustomUrlGrabberDefaultProperties(FItems[Index]);
end;

procedure TJvUrlGrabberDefaultPropertiesList.SetItems(Index: Integer;
  const Value: TJvCustomUrlGrabberDefaultProperties);
begin
  FItems[Index] := Value;
end;

function TJvUrlGrabberDefaultPropertiesList.GetItemsNamed(Name: string): TJvCustomUrlGrabberDefaultProperties;
var
  I: Integer;
begin
  I := 0;
  Result := nil;
  while (I < Count) and (Result = nil) do
  begin
    if Items[I].SupportedURLName = Name then
      Result := Items[I];
    Inc(I);
  end;
end;

//=== { TJvUrlGrabberDefPropEdTrick } ========================================

constructor TJvUrlGrabberDefPropEdTrick.Create(GrabberDefaults: TJvCustomUrlGrabberDefaultProperties);
begin
  if Assigned(GrabberDefaults) then
    FDefaultProperties := GrabberDefaults;
end;

//=== { TDFMPropertiesCollection } ===========================================

type
  // In order to store the Default Properties for every possible
  // grabber class, we have to deal with it ourselves. This is not
  // an easy task because the types of the default property holders
  // are not known while writing this class. Moreover, the streaming
  // system used by Delphi is not really well documented and the
  // only (not so) elegant way I found to stream the list of default
  // properties holder is to use WriteCollection and ReadCollection.
  // To do this, we need a collection but having TJvUrlGrabberDefaultPropertiesList
  // as a TCollection is too problematic because its members are
  // always descendents of TJvCustomUrlGrabberDefaultProperties.
  // So what I do here is to have a TCollection/TCollectionItem couple
  // that will be used to read and write the list from the DFM.
  // It works quite well and shouldn't need much improvement.
  TDFMPropertiesCollectionItem = class(TCollectionItem)
  private
    FOwnValue: Boolean;
    FValue: TJvCustomUrlGrabberDefaultProperties;
    FUrlType: string;
    procedure SetValue(const Value: TJvCustomUrlGrabberDefaultProperties);
    procedure SetUrlType(const Value: string);
  public
    destructor Destroy; override;
    constructor Create(Collection: TCollection); override;
  published
    property UrlType: string read FUrlType write SetUrlType;
    property Value: TJvCustomUrlGrabberDefaultProperties read FValue write SetValue;
  end;

  TDFMPropertiesCollection = class(TCollection)
  public
    constructor Create; reintroduce; overload;
    constructor Create(List: TJvUrlGrabberDefaultPropertiesList); reintroduce; overload;
  end;

constructor TDFMPropertiesCollection.Create;
begin
  inherited Create(TDFMPropertiesCollectionItem);
end;

constructor TDFMPropertiesCollection.Create(List: TJvUrlGrabberDefaultPropertiesList);
var
  I: Integer;
begin
  inherited Create(TDFMPropertiesCollectionItem);
  for I := 0 to List.Count - 1 do
  begin
    Add;
    TDFMPropertiesCollectionItem(Items[Count - 1]).Value := List.Items[I];
  end;
end;

//=== { TDFMPropertiesCollectionItem } =======================================

constructor TDFMPropertiesCollectionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TDFMPropertiesCollectionItem.Destroy;
begin
  if FOwnValue then
    FValue.Free;
  inherited Destroy;
end;

procedure TDFMPropertiesCollectionItem.SetValue(const Value: TJvCustomUrlGrabberDefaultProperties);
begin
  FValue := Value;
  FOwnValue := False;
  if Assigned(FValue) then
    FUrlType := FValue.GetSupportedURLName;
end;

procedure TDFMPropertiesCollectionItem.SetUrlType(const Value: string);
var
  I: Integer;
begin
  FUrlType := Value;
  if not Assigned(FValue) then
  begin
    for I := 0 to JvUrlGrabberClassList.Count - 1 do
      if JvUrlGrabberClassList[I].GetSupportedURLName = Value then
      begin
        FOwnValue := True;
        FValue := JvUrlGrabberClassList[I].GetDefaultPropertiesClass.Create(nil);
      end;
  end;
end;

//=== { TJvCustomUrlGrabberDefaultProperties } ===============================

constructor TJvCustomUrlGrabberDefaultProperties.Create(AOwner: TJvUrlGrabberDefaultPropertiesList);
begin
  inherited Create;
  FEditorTrick := TJvUrlGrabberDefPropEdTrick.Create(Self);

  FAgent := RsJediAgent;
  FUserName := '';
  FPassword := '';
  FFileName := RsDefaultOutputFileName;
  FOutputMode := omFile;
end;

destructor TJvCustomUrlGrabberDefaultProperties.Destroy;
begin
  FEditorTrick.Free;
  inherited Destroy;
end;

procedure TJvCustomUrlGrabberDefaultProperties.Assign(Source: TPersistent);
begin
  if Source is TJvCustomUrlGrabberDefaultProperties then
    with Source as TJvCustomUrlGrabberDefaultProperties do
    begin
      Self.Agent := Agent;
      Self.Port := Port;
      Self.Password := Password;
      Self.UserName := UserName;
      Self.FileName := FileName;
      Self.OutputMode := OutputMode;
    end
  else
    inherited Assign(Source);
end;

procedure TJvUrlGrabberDefaultPropertiesList.Read(Reader: TReader);
var
  I, J: Integer;
  TmpColl: TDFMPropertiesCollection;
begin
  // WARNING: The call to ReadValue is essential for the collection to
  // be read correctly. Somehow, WriteCollection writes something that
  // ReadCollection won't read on its own.
  Reader.ReadValue;

  TmpColl := TDFMPropertiesCollection.Create;
  try
    Reader.ReadCollection(TmpColl);
    for I := 0 to TmpColl.Count - 1 do
    begin
      for J := 0 to Count - 1 do
        if TDFMPropertiesCollectionItem(TmpColl.Items[I]).Value.GetSupportedURLName = Items[I].GetSupportedURLName then
          Items[I].Assign(TDFMPropertiesCollectionItem(TmpColl.Items[I]).Value);
    end;
  finally
    TmpColl.Free;
  end;
end;

procedure TJvUrlGrabberDefaultPropertiesList.Write(Writer: TWriter);
var
  TmpColl: TDFMPropertiesCollection;
begin
  TmpColl := TDFMPropertiesCollection.Create(Self);
  try
    Writer.WriteCollection(TmpColl);
  finally
    TmpColl.Free;
  end;
end;

procedure TJvUrlListGrabber.SetMaxSimultaneousGrabbers(
  const Value: Integer);
begin
  if FMaxSimultaneousGrabbers <> Value then
  begin
    FMaxSimultaneousGrabbers := Value;
    if FMaxSimultaneousGrabbers < 0 then
      FMaxSimultaneousGrabbers := 0;
  end;
end;

{ TJvUrlGrabberTimeout }

procedure TJvUrlGrabberTimeout.Assign(Source: TPersistent);
begin
  if Source is TJvUrlGrabberTimeout Then
  begin
    FConnect := TJvUrlGrabberTimeout(Source).Connect;
    FReceive := TJvUrlGrabberTimeout(Source).Receive;
    FSend := TJvUrlGrabberTimeout(Source).Send;
  end
  else 
  begin
    inherited;
  end;
end;  

constructor TJvUrlGrabberTimeOut.Create;
begin
  inherited Create;

  FConnect := -1;
  FReceive := -1;
  FSend := -1;
end;

procedure TJvUrlGrabberTimeOut.SetupSession(ASession: HINTERNET);
  procedure SetTimeout(Option: Cardinal; Value: Integer);
  begin
    case Value of
      -1:
        ; // nothing, leave default value
      0:
      begin
        Value := Integer($FFFFFFFF);
        InternetSetOption(ASession, Option, @Value, SizeOf(Value));
      end
      else
        if Value > 0 then
          InternetSetOption(ASession, Option, @Value, SizeOf(Value));
    end;
  end;
begin
   SetTimeout(INTERNET_OPTION_CONNECT_TIMEOUT, Connect);
   SetTimeout(INTERNET_OPTION_RECEIVE_TIMEOUT, Receive);
   SetTimeout(INTERNET_OPTION_SEND_TIMEOUT, Send);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  FreeAndNil(GJvUrlGrabberClassList);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

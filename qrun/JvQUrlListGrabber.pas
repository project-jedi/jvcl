{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUrlListGrabber.Pas, released on 2003-08-04.

The Initial Developer of the Original Code is Olivier Sannier [obones@meloo.com]
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQUrlListGrabber;

interface

{$HPPEMIT '#pragma link "wininet.lib"'}

uses
  Windows, Classes, SysUtils, Contnrs,
  JvQComponent, JvQTypes;

type
  // early declarations
  TJvUrlListGrabber = class;
  TJvCustomUrlGrabber = class;
  TJvUrlGrabberList = class;
  TJvUrlGrabberDefaultPropertiesList = class;

  // A Grabber index, defined as a new type to allow to give it
  // a specific property editor 
  TJvUrlGrabberIndex = type Integer;

  // The type of the events triggered when one of the grabbers
  // has triggred its own event to indicate a change in its state
  TJvGrabberNotifyEvent = procedure(Sender: TJvUrlListGrabber; Grabber: TJvCustomUrlGrabber) of object;

  // The exception raised by TJvUrlListGrabber when no grabber claimed it was capable
  // of handling a given URL. This is only raised if DefaultGrabberIndex is -1
  ENoGrabberForUrl = class(Exception);

  // This component allows the user to specify a list of URLs to be
  // grabbed and then start grabbing. All the grab operations will be done
  // in parallel in the background, leaving the user's application free
  // to continue its operations
  TJvUrlListGrabber = class(TJvComponent)
  protected
    FOnClosed: TJvGrabberNotifyEvent;
    FOnReceiving: TJvGrabberNotifyEvent;
    FOnResolving: TJvGrabberNotifyEvent;
    FOnReceived: TJvGrabberNotifyEvent;
    FOnConnecting: TJvGrabberNotifyEvent;
    FOnRequest: TJvGrabberNotifyEvent;
    FOnConnected: TJvGrabberNotifyEvent;
    FOnResolved: TJvGrabberNotifyEvent;
    FOnSent: TJvGrabberNotifyEvent;
    FOnClosing: TJvGrabberNotifyEvent;
    FOnSending: TJvGrabberNotifyEvent;
    FCleanupThreshold: Cardinal;
    FGrabbers : TJvUrlGrabberList;
    FURLs: TStringList;
    FDefaultGrabberIndex: TJvUrlGrabberIndex;
    FDefaultGrabbersProperties: TJvUrlGrabberDefaultPropertiesList;
    // gets/sets the URLs property, assigning the given strings
    // to the internal FURLs field
    function GetUrls: TStrings;
    procedure SetUrls(const Value: TStrings);

    // sets the Default Grabber value, ensuring that it doesn't go
    // below -1 or above the number of registered grabber classes
    // if you try to set the value above the last index in the
    // JvUrlGrabberClassList, then the value will be set to -1.
    // The same goes if you set a value below -1.
    procedure SetDefaultGrabberIndex(const Value: TJvUrlGrabberIndex);

    // returns the grabber associated with the given index
    function GetGrabbers(const Index: Integer): TJvCustomUrlGrabber;
    
    // Called whenever the list of Urls has changed
    procedure URLsChange(Sender : TObject);

    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    // cleans up the internal list of grabbers
    procedure Cleanup;

    // starts all the grabbers
    procedure StartAll;

    // stops all the grabbers
    procedure StopAll;

    // the Grabber objects associated with the Urls
    property Grabbers[const Index : Integer]: TJvCustomUrlGrabber read GetGrabbers;
  published
    // the index of the default grabber to use, if any
    property DefaultGrabberIndex: TJvUrlGrabberIndex read FDefaultGrabberIndex write SetDefaultGrabberIndex default -1;

    // the cleanup threshold. When the difference between Urls.Count
    // and the internal Grabber count is greater than this value
    // the process of cleaning if launched. This can take some time
    // and this is why it's not done every time
    property CleanupThreshold: Cardinal read FCleanupThreshold write FCleanupThreshold default 10;

    // The Urls to grab
    property URLs: TStrings read GetURLs write SetUrls;
    // The default properties for each family of grabber
    property DefaultGrabbersProperties : TJvUrlGrabberDefaultPropertiesList read FDefaultGrabbersProperties;
    // Events
    property OnResolvingName: TJvGrabberNotifyEvent read FOnResolving write FOnResolving;
    property OnNameResolved: TJvGrabberNotifyEvent read FOnResolved write FOnResolved;
    property OnConnectingToServer: TJvGrabberNotifyEvent read FOnConnecting write FOnConnecting;
    property OnConnectedToServer: TJvGrabberNotifyEvent read FOnConnected write FOnConnected;
    property OnSendingRequest: TJvGrabberNotifyEvent read FOnSending write FOnSending;
    property OnRequestSent: TJvGrabberNotifyEvent read FOnSent write FOnSent;
    property OnRequestComplete: TJvGrabberNotifyEvent read FOnRequest write FOnRequest;
    property OnReceivingResponse: TJvGrabberNotifyEvent read FOnReceiving write FOnReceiving;
    property OnResponseReceived: TJvGrabberNotifyEvent read FOnReceived write FOnReceived;
    property OnClosingConnection: TJvGrabberNotifyEvent read FOnClosing write FOnClosing;
    property OnConnectionClosed   : TJvGrabberNotifyEvent read FOnClosed write FOnClosed;
//    property OnRedirect: TGrabberNotifyEvent read FOnRedirect write FOnRedirect;
//    property OnStateChange: TGrabberNotifyEvent read FOnStateChange write FOnStateChange;
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
  protected
    // agent to impersonate
    FAgent: string;
    // user information
    FUserName: string;
    FPassword: string;
    // filename to use
    FFileName: TFileName;
    // output mode (stream or file)
    FOutputMode: TJvOutputMode;
    // The user-friendly name of the supported URL type
    function GetSupportedURLName: string; virtual; abstract;
    // The agent to impersonate
    property Agent: string read FAgent write FAgent;
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
    function GetItemsNamed(Name: string): TJvCustomUrlGrabberDefaultProperties;
  protected
    FItems: TObjectList;
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
  TJvGrabberStatus = (gsStopped, gsConnecting, gsGrabbing);

  // The exception triggered if someone tries to set the Url property while the
  // grabber is not stopped
  EGrabberNotStopped = class(Exception);

  // The event type used when a grabbing has had some progress
  TJvUrlGrabberProgressEvent = procedure(Sender: TObject; Position, TotalSize: Int64; Url: string; var Continue: Boolean) of object;

  // The ancestor of all the Url Grabbers that declares the required
  // methods that a grabber must provide.
  // Do not instanciate a TJvCustomUrlGrabber directly, simply use one
  // of its descendants. This family of classes is used by
  // TJvUrlListGrabber to allow downloading a list of URLs but can
  // also be used on their own to grad one URL of a given type.
  TJvCustomUrlGrabber = class(TJvComponent)
  protected
    // the thread that will grab for us
    FUrlGrabberThread: TJvCustomUrlGrabberThread;
    // events
    FOnDoneFile   : TJvDoneFileEvent;           // file is done
    FOnDoneStream : TJvDoneStreamEvent;         // stream is done
    FOnError      : TJvErrorEvent;              // error occured
    FOnProgress   : TJvUrlGrabberProgressEvent; // download progressed a bit
    FOnClosed     : TNotifyEvent;               // connection is closed
    FOnReceiving  : TNotifyEvent;               // beginning to receive
    FOnReceived   : TNotifyEvent;               // end of reception
    FOnConnecting : TNotifyEvent;               // beginning of connection
    FOnResolving  : TNotifyEvent;               // beginning of resolving URL
    FOnRedirect   : TNotifyEvent;               // redirection happened
    FOnConnected  : TNotifyEvent;               // now connected to host
    FOnStateChange: TNotifyEvent;               // state of connection changed
    FOnResolved   : TNotifyEvent;               // name has been resolved
    FOnClosing    : TNotifyEvent;               // beginning of close of connection
    FOnRequest    : TNotifyEvent;               // sending a request
    FOnSent       : TNotifyEvent;               // data sent
    FOnSending    : TNotifyEvent;               // beginning to send data

    // current status of the grabber
    FStatus: TJvGrabberStatus;

    // URL to grab
    FUrl: string;

    // the stream to grab into.
    FStream: TMemoryStream;

    // agent to impersonate
    FAgent: string;

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

    // Event callers
    procedure DoError(ErrorMsg: string);
    procedure DoProgress(Position: Integer; var Continue: boolean);
    procedure DoEnded;
    procedure DoClosed;
    
    function GetGrabberThreadClass: TJvCustomUrlGrabberThreadClass; virtual; abstract;

    procedure SetUrl(Value: string);
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties); reintroduce; overload; virtual;
    destructor Destroy; override;

    // this function must return True if the given URL can be grabbed
    // by the class being asked. It returns False otherwise
    // It MUST be overriden in the derived classes but cannot be abstract
    // because of BCB compatibility issues
    class function CanGrab(const Url: string): Boolean; virtual;
    
    // This function returns the class of a property holder to
    // be displayed in the object inspector. This property holder
    // will be used by TJvUrlListGrabber to let the user specify default
    // properties and will be passed to this class when created to
    // handle a specific URL.
    // It MUST be overriden in the derived classes but cannot be abstract
    // because of BCB compatibility issues
    class function GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass; virtual;

    // this function must return a user displayable string indicating
    // the type of URL that class of grabber supports.
    // It MUST be overriden in the derived classes but cannot be abstract
    // because of BCB compatibility issues
    class function GetSupportedURLName: string; virtual;

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

    // the user name and password to use for authentication
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;

    // the name of the file to write to if OutputMode is omFile
    property FileName: TFileName read FFileName write FFileName;

    // The output mode
    property OutputMode: TJvOutputMode read FOutputMode write FOutputMode default omFile;

    // The agent to impersonate
    property Agent: string read FAgent write FAgent;

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
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;

  // A thread that will grab the given URL in the background
  // this is the ancestor of all the grabber threads, and there
  // should be as many descendants as there are TJvCustomUrlGrabber
  // descendants.
  TJvCustomUrlGrabberThread = class(TThread)
  protected
    FErrorText: string; // the error string received from the server
    FGrabber: TJvCustomUrlGrabber;
    FStatus: DWORD;
    FContinue: boolean;
    
    procedure Error;
    procedure Ended;
    procedure Progress;
    procedure ParseUrl(Value: string; Protocol: string; var Host: string; var FileName: string);
  public
    constructor Create(Grabber: TJvCustomUrlGrabber); virtual;
    procedure DoProgress;

    property Status: DWORD read FStatus write FStatus;
  end;

  // A list of instances of TJvUrlGrabber descendants
  // This is used internally by TJvUrlListGrabber to keep track of
  // the objects in charge of every URLs it has to grab
  TJvUrlGrabberList = class(TObjectList)
  protected
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
  protected
    function GetItem(Index: Integer): TJvCustomUrlGrabberClass;
    procedure SetItem(Index: Integer; const AGrabberClass: TJvCustomUrlGrabberClass);
  public
    procedure Populate(DefaultPropertiesList: TJvUrlGrabberDefaultPropertiesList);
    function Add(AGrabberClass: TJvCustomUrlGrabberClass): Integer;
    procedure Insert(Index: Integer; AGrabberClass: TJvCustomUrlGrabberClass);
    function CreateFor(Owner: TComponent; Url: string; DefaultPropertiesList: TJvUrlGrabberDefaultPropertiesList): TJvCustomUrlGrabber;
    property Items[Index: Integer]: TJvCustomUrlGrabberClass read GetItem write SetItem; default;
  end;

function JvUrlGrabberClassList: TJvUrlGrabberClassList;

implementation

uses
  JvQConsts, JvQResources, JvQFinalize,
  // JvUrlGrabbers MUST be included here so that the grabbers
  // it contains are registered before any JvUrlListGrabber
  // component reads its properties.
  JvQUrlGrabbers;

const
  sUnitName = 'JvUrlListGrabber';

var
  // the global object to contain the list of registered
  // url grabber classes
  GJvUrlGrabberClassList: TJvUrlGrabberClassList = nil;

function JvUrlGrabberClassList: TJvUrlGrabberClassList;
begin
  if not Assigned(GJvUrlGrabberClassList) then
  begin
    // create the object
    GJvUrlGrabberClassList := TJvUrlGrabberClassList.Create;
    AddFinalizeObjectNil(sUnitName, TObject(GJvUrlGrabberClassList));
  end;
  Result := GJvUrlGrabberClassList;
end;

//=== TJvUrlListGrabber ======================================================

constructor TJvUrlListGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultGrabbersProperties := TJvUrlGrabberDefaultPropertiesList.Create(Self);
  FGrabbers := TJvUrlGrabberList.Create(True);
  FURLs := TStringList.Create;
  FURLs.OnChange := URLsChange;
  FDefaultGrabberIndex := -1;
  FCleanupThreshold := 10;
end;

destructor TJvUrlListGrabber.Destroy;
begin
  FURLs.Free;
  FGrabbers.Free;
  FDefaultGrabbersProperties.Free;
  inherited Destroy;
end;

procedure TJvUrlListGrabber.Cleanup;
var
  I: Integer;
begin
  // try to find each created grabber in the string list
  // if not found, mark the object as nil which in turn
  // will delete it
  for I := 0 to FGrabbers.Count - 1 do
    if FUrls.IndexOfObject(FGrabbers[I]) = -1 then
      FGrabbers[I] := nil;
  // pack the list
  FGrabbers.Pack;
end;

function TJvUrlListGrabber.GetGrabbers(const Index: Integer): TJvCustomUrlGrabber;
begin
  Result := TJvCustomUrlGrabber(FURLs.Objects[Index]);
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

function TJvUrlListGrabber.GetUrls: TStrings;
begin
  Result := FURLs;
end;

procedure TJvUrlListGrabber.SetUrls(const Value: TStrings);
begin
  FURLs.Assign(Value);
end;

procedure TJvUrlListGrabber.StartAll;
var
  I: Integer;
begin
  for I := 0 to FUrls.Count - 1 do
    Grabbers[I].Start;
end;

procedure TJvUrlListGrabber.StopAll;
var
  I: Integer;
begin
  for I := 0 to FUrls.Count - 1 do
    Grabbers[I].Stop;
end;

procedure TJvUrlListGrabber.URLsChange(Sender: TObject);
var
  I: Integer;
  TmpGrabber: TJvCustomUrlGrabber;
begin
  for I := 0 to FUrls.Count - 1 do
  begin
    if not Assigned(FUrls.Objects[I]) then
    begin
      TmpGrabber := JvUrlGrabberClassList.CreateFor(Self, FUrls[I], FDefaultGrabbersProperties);
      if Assigned(TmpGrabber) then
        FUrls.Objects[I] := TmpGrabber
      else
      if DefaultGrabberIndex > -1 then
        FUrls.Objects[I] := JvUrlGrabberClassList[DefaultGrabberIndex].Create(Self, FUrls[I], FDefaultGrabbersProperties.Items[DefaultGrabberIndex])
      else
        raise ENoGrabberForUrl.CreateResFmt(@RsENoGrabberForUrl, [FUrls[I]]);

      // add in the list of owned objects
      FGrabbers.Add(TJvCustomUrlGrabber(FUrls.Objects[I]));
      if Cardinal(FGrabbers.Count - FUrls.Count) > FCleanupThreshold then
        Cleanup;
    end;
  end;
end;

procedure TJvUrlListGrabber.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('DefaultGrabbersPropertiesList', DefaultGrabbersProperties.Read, DefaultGrabbersProperties.Write, True);
end;

//=== TJvCustomUrlGrabber ==========================================================

constructor TJvCustomUrlGrabber.Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties);
begin
  inherited Create(AOwner);
  FUrl := AUrl;
  FUrlGrabberThread := nil;

  // get values from the default properties
  Agent := DefaultProperties.Agent;
  UserName := DefaultProperties.UserName;
  Password := DefaultProperties.Password;
  FileName := DefaultProperties.FileName;
  OutputMode := DefaultProperties.OutputMode;
end;

constructor TJvCustomUrlGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Set default properties
  Agent := JediAgent;
  UserName := '';
  Password := '';
  FileName := DefaultOutputFileName;
  OutputMode := omFile;
end;

destructor TJvCustomUrlGrabber.Destroy;
begin
  FUrlGrabberThread.Free;
  inherited Destroy;
end;

class function TJvCustomUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  // useless implementation required for BCB compatibility as
  // C++ doesn't support abstract virtual class methods
  Result := False;
end;

procedure TJvCustomUrlGrabber.SetUrl(Value: string);
begin
  if Status = gsStopped then
    FUrl := Value
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
  FStream.Position := 0;
  if FOutputMode = omStream then
  begin
    if Assigned(FOnDoneStream) then
      FOnDoneStream(Self, FStream, FStream.Size, FUrl);
  end
  else
  begin
    FStream.SaveToFile(FFileName);
    if Assigned(FOnDoneFile) then
      FOnDoneFile(Self, FFileName, FStream.Size, FUrl);
  end;
end;

procedure TJvCustomUrlGrabber.DoError(ErrorMsg: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, ErrorMsg);
end;

procedure TJvCustomUrlGrabber.DoProgress(Position: Integer; var Continue: boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Position, FSize, Url, Continue);
end;

class function TJvCustomUrlGrabber.GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass;
begin
  // useless implementation for BCB Compatibility
  Result := nil;
end;

procedure TJvCustomUrlGrabber.Start;
begin
  // Delete the existing thread, if any. Will ask it to terminate
  FreeAndNil(FUrlGrabberThread);

  // Create a new thread
  FUrlGrabberThread := GetGrabberThreadClass.Create(Self);
  FUrlGrabberThread.Resume;
end;

procedure TJvCustomUrlGrabber.Stop;
begin
  FUrlGrabberThread.Terminate;
end;

class function TJvCustomUrlGrabber.GetSupportedURLName: string;
begin
  // Useless implementation for BCB
  Result := '';
end;

//=== TJvUrlGrabberList ======================================================

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

//=== TJvCustomUrlGrabberClassList =================================================

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

//=== TJvCustomUrlGrabberThread ====================================================

constructor TJvCustomUrlGrabberThread.Create(Grabber: TJvCustomUrlGrabber);
begin
  inherited Create(True);
  FContinue := True;
  FGrabber := Grabber;
end;

procedure TJvCustomUrlGrabberThread.DoProgress;
begin
  Synchronize(Progress);
end;

procedure TJvCustomUrlGrabberThread.Ended;
begin
  FGrabber.DoEnded;
end;

procedure TJvCustomUrlGrabberThread.Error;
begin
  FGrabber.DoError(FErrorText);
end;

procedure TJvCustomUrlGrabberThread.Progress;
begin
  FGrabber.DoProgress(FGrabber.BytesRead, FContinue);
end;

procedure TJvCustomUrlGrabberThread.ParseUrl(Value: string; Protocol: string;
  var Host: string; var FileName: string);
begin
  Host := '';
  FileName := '';
  if Pos(UpperCase(Protocol), UpperCase(Value)) <> 0 then
    Value := Copy(Value, 8, Length(Value));
  if Pos('/', Value) <> 0 then
  begin
    Host := Copy(Value, 1, Pos('/', Value) - 1);
    FileName := Copy(Value, Pos('/', Value) + 1, Length(Value));
  end
  else
    Host := Value;
end;

//=== TJvUrlGrabberDefaultPropertiesList ===============================

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

//=== TJvUrlGrabberDefPropEdTrick ============================================

constructor TJvUrlGrabberDefPropEdTrick.Create(GrabberDefaults: TJvCustomUrlGrabberDefaultProperties);
begin
  if Assigned(GrabberDefaults) then
    FDefaultProperties := GrabberDefaults;
end;

//=== TJvCustomUrlGrabberDefaultProperties =========================================

procedure TJvCustomUrlGrabberDefaultProperties.Assign(Source: TPersistent);
begin
  if Source is TJvCustomUrlGrabberDefaultProperties then
    with Source as TJvCustomUrlGrabberDefaultProperties do
    begin
      Self.Agent := Agent;
      Self.Password := Password;
      Self.UserName := UserName;
      Self.FileName := FileName;
      Self.OutputMode := OutputMode;
    end
  else
    inherited Assign(Source);
end;

constructor TJvCustomUrlGrabberDefaultProperties.Create(AOwner: TJvUrlGrabberDefaultPropertiesList);
begin
  inherited Create;
  FEditorTrick := TJvUrlGrabberDefPropEdTrick.Create(Self);

  FFileName := DefaultOutputFileName;
  FAgent := JediAgent;
  FUserName := '';
  FPassword := '';
  FFileName := DefaultOutputFileName;
  FOutputMode := omFile;
end;

destructor TJvCustomUrlGrabberDefaultProperties.Destroy;
begin
  FEditorTrick.Free;
  inherited Destroy;
end;

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
    FOwnValue: boolean;

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
  for I := 0 to List.Count -1 do
  begin
    Add;
    TDFMPropertiesCollectionItem(Items[Count-1]).Value := List.Items[I];
  end;
end;

constructor TDFMPropertiesCollectionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TDFMPropertiesCollectionItem.Destroy;
begin
  if FOwnValue then
    FValue.Free;

  inherited;
end;

procedure TDFMPropertiesCollectionItem.SetValue(
  const Value: TJvCustomUrlGrabberDefaultProperties);
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

procedure TJvUrlGrabberDefaultPropertiesList.Read(Reader: TReader);
var
  I, J : Integer;
  TmpColl : TDFMPropertiesCollection;
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
  TmpColl : TDFMPropertiesCollection;
begin
  TmpColl := TDFMPropertiesCollection.Create(Self);
  try
    Writer.WriteCollection(TmpColl);
  finally
    TmpColl.Free;
  end;
end;

initialization

finalization
  FinalizeUnit(sUnitName);

end.

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

Last Modified: 2003-11-02

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvUrlListGrabber;

interface

uses
  Windows, Classes, SysUtils, Contnrs,
  JvComponent, JvTypes;

type
  // early declarations
  TJvUrlListGrabber = class;
  TJvUrlGrabber = class;
  TJvUrlGrabberList = class;
  TJvUrlGrabberDefaultPropertiesList = class;


  // A Grabber index, defined as a new type to allow to give it
  // a specific property editor 
  TJvUrlGrabberIndex = type Integer;

  // The type of the events triggered when one of the grabbers
  // has triggred its own event to indicate a change in its state
  TJvGrabberNotifyEvent = procedure(Sender: TJvUrlListGrabber; Grabber: TJvUrlGrabber) of object;

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
    function GetGrabbers(const Index: Integer): TJvUrlGrabber;
    // Called whenever the list of Urls has changed
    procedure URLsChange(Sender : TObject);
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
    property Grabbers[const Index : Integer]: TJvUrlGrabber read GetGrabbers;
  published
    // the index of the default grabber to use, if any
    property DefaultGrabberIndex: TJvUrlGrabberIndex read FDefaultGrabberIndex write SetDefaultGrabberIndex default -1;
    // the cleanup threshold. When the difference between Urls.Count
    // and the internal Grabber count is greater than this value
    // the process of cleaning if launched. This can take some time
    // and this is why it's done every time
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
  TJvUrlGrabberThread = class;
  TJvUrlGrabberDefaultProperties = class;
  TJvUrlGrabberThreadClass = class of TJvUrlGrabberThread;

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
  TJvUrlGrabberDefPropEdTrick = class(TJvPersistent)
  private
    FDefaultProperties: TJvUrlGrabberDefaultProperties;
  public
    constructor Create(GrabberDefaults: TJvUrlGrabberDefaultProperties); reintroduce; virtual;
  published
    property DefaultProperties: TJvUrlGrabberDefaultProperties read FDefaultProperties;
  end;

  // A container for Default properties, and a list of such
  // containers
  TJvUrlGrabberDefaultProperties = class(TJvPersistent)
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

    property EditorTrick: TJvUrlGrabberDefPropEdTrick read FEditorTrick;
    property SupportedURLName: string read GetSupportedURLName;
  published
    // the name of the file to write to if OutputMode is omFile
    property FileName: TFileName read FFileName write FFileName;
    // The output mode
    property OutputMode: TJvOutputMode read FOutputMode write FOutputMode default omStream;
  end;

  TJvUrlGrabberDefaultPropertiesClass = class of TJvUrlGrabberDefaultProperties;

  TJvUrlGrabberDefaultPropertiesList = class(TJvPersistent)
  private
    function GetItemsNamed(Name: string): TJvUrlGrabberDefaultProperties;
  protected
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TJvUrlGrabberDefaultProperties;
    procedure SetItems(Index: Integer; const Value: TJvUrlGrabberDefaultProperties);
  public
    constructor Create(AOwner: TJvUrlListGrabber); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Item: TJvUrlGrabberDefaultProperties);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJvUrlGrabberDefaultProperties read GetItems write SetItems;
    property ItemsNamed[Name: string]: TJvUrlGrabberDefaultProperties read GetItemsNamed; default;
  end;

  // the status of a grabber
  TJvGrabberStatus = (gsStopped, gsConnecting, gsGrabbing);

  // The main class. It is the ancestor of all the Url Grabbers
  // and declares the required methods that a grabber must provide
  // Do not instanciate a TJvUrlGrabber directly, simply use one
  // of its descendants. This family of classes is used by
  // TJvUrlListGrabber to allow downloading a list of URLs

  TJvUrlGrabber = class(TObject)
  protected
    // the thread that will grab for us
    FUrlGrabberThread: TJvUrlGrabberThread;
    // events
    FOnDoneFile: TJvDoneFileEvent; // file is done
    FOnDoneStream: TJvDoneStreamEvent; // stream is done
    FOnError: TJvErrorEvent; // error occured
    FOnProgress: TJvFTPProgressEvent; // download progressed a bit
    FOnClosed: TNotifyEvent; // connection is closed
    FOnReceiving: TNotifyEvent; // beginning to receive
    FOnReceived: TNotifyEvent; // end of reception
    FOnConnecting: TNotifyEvent; // beginning of connection
    FOnResolving: TNotifyEvent; // beginning of resolving URL
    FOnRedirect: TNotifyEvent; // redirection happened
    FOnConnected: TNotifyEvent; // now connected to host
    FOnStateChange: TNotifyEvent; // state of connection changed
    FOnResolved: TNotifyEvent; // name has been resolved
    FOnClosing: TNotifyEvent; // beginning of close of connection
    FOnRequest: TNotifyEvent; // sending a request
    FOnSent: TNotifyEvent; // data sent
    FOnSending: TNotifyEvent; // beginning to send data
    // current status of the grabber
    FStatus: TJvGrabberStatus;
    // URL to grab
    FUrl: string;
    // the stream to grab into.
    FStream: TMemoryStream;
    FTotalBytes: Int64;
    FBytesRead: Int64;
    // agent to impersonate
    FAgent: string;
    // user information
    FUserName: string;
    FPassword: string;
    // filename to use
    FFileName: TFileName;
    // output mode (stream or file)
    FOutputMode: TJvOutputMode;
    procedure DoError(ErrorMsg: string);
    procedure DoProgress(Status: DWORD);
    procedure DoEnded;
    procedure DoClosed;
    function GetGrabberThreadClass: TJvUrlGrabberThreadClass; virtual; abstract;
  public
    constructor Create(AUrl: string; DefaultProperties: TJvUrlGrabberDefaultProperties); virtual;
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
    class function GetDefaultPropertiesClass: TJvUrlGrabberDefaultPropertiesClass; virtual;
    // Asks to Start to grab the URL
    procedure Start; virtual;
    // Asks to Stop to grab the URL
    procedure Stop; virtual;
    // The status of the grab
    property Status: TJvGrabberStatus read FStatus;
    // the Url being grabbed
    property Url: string read FUrl;
    // the user name and password to use for authentication
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    // the name of the file to write to if OutputMode is omFile
    property FileName: TFileName read FFileName write FFileName;
    // The output mode
    property OutputMode: TJvOutputMode read FOutputMode write FOutputMode default omStream;
    // The agent to impersonate
    property Agent: string read FAgent write FAgent;
    // Events
    property OnDoneFile: TJvDoneFileEvent read FOnDoneFile write FOnDoneFile;
    property OnDoneStream: TJvDoneStreamEvent read FOnDoneStream write FOnDoneStream;
    property OnError: TJvErrorEvent read FOnError write FOnError;
    property OnProgress: TJvFTPProgressEvent read FOnProgress write FOnProgress;
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
  // should be as many descendants as there are TJvUrlGrabber descendants.
  TJvUrlGrabberThread = class(TThread)
  protected
    FErrorText: string; // the error string received from the server
    FGrabber: TJvUrlGrabber;
    FStatus: DWORD;
    procedure Error;
    procedure Ended;
    procedure Progress;
    procedure ParseUrl(Value: string; Protocol: string; var Host: string; var FileName: string);
  public
    constructor Create(Grabber: TJvUrlGrabber); virtual;
    procedure DoProgress;

    property Status: DWORD read FStatus write FStatus;
  end;

  // A list of instances of TJvUrlGrabber descendants
  // This is used internally by TJvUrlListGrabber to keep track of
  // the objects in charge of every URLs it has to grab

  TJvUrlGrabberList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TJvUrlGrabber;
    procedure SetItem(Index: Integer; const AGrabber: TJvUrlGrabber);
  public
    function Add(AGrabber: TJvUrlGrabber): integer;
    procedure Insert(Index: integer; AGrabber: TJvUrlGrabber);
    property Items[Index: Integer]: TJvUrlGrabber read GetItem write SetItem; default;
  end;

  TJvUrlGrabberClass = class of TJvUrlGrabber;

  // A list of classes inheriting from TJvUrlGrabber
  // This is the type of list used by the JvUrlGrabberClassList
  // function that returns all the registered classes.
  // This list is then used by TJvUrlListGrabber to determine which
  // class is best suited for handling a given URL

  TJvUrlGrabberClassList = class(TClassList)
  protected
    function GetItem(Index: Integer): TJvUrlGrabberClass;
    procedure SetItem(Index: Integer; const AGrabberClass: TJvUrlGrabberClass);
  public
    procedure Populate(DefaultPropertiesList: TJvUrlGrabberDefaultPropertiesList);
    function Add(AGrabberClass: TJvUrlGrabberClass): integer;
    procedure Insert(Index: integer; AGrabberClass: TJvUrlGrabberClass);
    function CreateFor(Url: string; DefaultPropertiesCollection: TJvUrlGrabberDefaultPropertiesList): TJvUrlGrabber;
    property Items[Index: Integer]: TJvUrlGrabberClass read GetItem write SetItem; default;
  end;


function JvUrlGrabberClassList: TJvUrlGrabberClassList;

implementation

uses
  JvResources, JvFinalize;

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

constructor TJvUrlListGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultGrabbersProperties := TJvUrlGrabberDefaultPropertiesList.Create(Self);
  FGrabbers := TJvUrlGrabberList.Create(True);
  FURLs := TStringList.Create;
  TStringList(FURLs).OnChange := URLsChange;
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

function TJvUrlListGrabber.GetGrabbers(const Index: Integer): TJvUrlGrabber;
begin
  Result := TJvUrlGrabber(FURLs.Objects[Index]);
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
  TmpGrabber: TJvUrlGrabber;
begin
  for I := 0 to FUrls.Count - 1 do
  begin
    if not Assigned(FUrls.Objects[I]) then
    begin
      TmpGrabber := JvUrlGrabberClassList.CreateFor(FUrls[I], FDefaultGrabbersProperties);
      if Assigned(TmpGrabber) then
        FUrls.Objects[I] := TmpGrabber
      else
      if DefaultGrabberIndex > -1 then
        FUrls.Objects[I] := JvUrlGrabberClassList[DefaultGrabberIndex].Create(FUrls[I], FDefaultGrabbersProperties.Items[DefaultGrabberIndex])
      else
        raise ENoGrabberForUrl.CreateFmt(RsENoGrabberForUrl, [FUrls[I]]);

      // add in the list of owned objects
      FGrabbers.Add(TJvUrlGrabber(FUrls.Objects[I]));
      if Cardinal(FGrabbers.Count - FUrls.Count) > FCleanupThreshold then
        Cleanup;
    end;
  end;
end;

//=== TJvUrlGrabber ==========================================================

constructor TJvUrlGrabber.Create(AUrl: string; DefaultProperties: TJvUrlGrabberDefaultProperties);
begin
  inherited Create;
  FUrl := AUrl;
  FUrlGrabberThread := nil;

  // get values from the default properties
  Agent := DefaultProperties.Agent;
  UserName := DefaultProperties.UserName;
  Password := DefaultProperties.Password;
  FileName := DefaultProperties.FileName;
  OutputMode := DefaultProperties.OutputMode;
end;

destructor TJvUrlGrabber.Destroy;
begin
  FUrlGrabberThread.Free;
  inherited Destroy;
end;

class function TJvUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  // useless implementation required for BCB compatibility as
  // C++ doesn't support abstract virtual class methods
  Result := False;
end;

procedure TJvUrlGrabber.DoClosed;
begin
  if Assigned(FOnClosed) then
    FOnClosed(Self);
end;

procedure TJvUrlGrabber.DoEnded;
begin
  FStream.Position := 0;
  if FOutputMode = omStream then
  begin
    if Assigned(FOnDoneStream) then
      FOnDoneStream(Self, FStream, FStream.Size, FUrl)
  end
  else
  begin
    FStream.SaveToFile(FFileName);
    if Assigned(FOnDoneFile) then
      FOnDoneFile(Self, FFileName, FStream.Size, FUrl);
  end;
end;

procedure TJvUrlGrabber.DoError(ErrorMsg: string);
begin
  if Assigned(FOnError) then
    FOnError(self, ErrorMsg);
end;

procedure TJvUrlGrabber.DoProgress(Status: DWORD);
begin
  if Assigned(FOnProgress) then
    FOnProgress(self, Status, FUrl);
end;

class function TJvUrlGrabber.GetDefaultPropertiesClass: TJvUrlGrabberDefaultPropertiesClass;
begin
  // useless implementation for BCB Compatibility
  Result := nil;
end;

procedure TJvUrlGrabber.Start;
begin
  FUrlGrabberThread := GetGrabberThreadClass.Create(Self);
  FUrlGrabberThread.Resume;
end;

procedure TJvUrlGrabber.Stop;
begin
  FUrlGrabberThread.Terminate;
end;

//=== TJvUrlGrabberList ======================================================

function TJvUrlGrabberList.Add(AGrabber: TJvUrlGrabber): integer;
begin
  Result := inherited Add(AGrabber);
end;

function TJvUrlGrabberList.GetItem(Index: Integer): TJvUrlGrabber;
begin
  Result := TJvUrlGrabber(inherited Items[Index]);
end;

procedure TJvUrlGrabberList.Insert(Index: Integer; AGrabber: TJvUrlGrabber);
begin
  inherited Insert(Index, AGrabber);
end;

procedure TJvUrlGrabberList.SetItem(Index: Integer; const AGrabber: TJvUrlGrabber);
begin
  inherited Items[Index] := AGrabber;
end;

//=== TJvUrlGrabberClassList =================================================

function TJvUrlGrabberClassList.Add(AGrabberClass: TJvUrlGrabberClass): integer;
begin
  Result := inherited Add(AGrabberClass);
end;

function TJvUrlGrabberClassList.CreateFor(Url: string;
  DefaultPropertiesCollection: TJvUrlGrabberDefaultPropertiesList): TJvUrlGrabber;
var
  I: Integer;
begin
  I := 0;
  Result := nil;
  while (I < Count) and not Assigned(Result) do
  begin
    if Items[I].CanGrab(Url) then
      Result := Items[I].Create(Url, DefaultPropertiesCollection.Items[I]);
    Inc(I);
  end;
end;

function TJvUrlGrabberClassList.GetItem(Index: Integer): TJvUrlGrabberClass;
begin
  Result := TJvUrlGrabberClass(inherited Items[Index]);
end;

procedure TJvUrlGrabberClassList.Insert(Index: Integer;
  AGrabberClass: TJvUrlGrabberClass);
begin
  inherited Insert(Index, AGrabberClass);
end;

procedure TJvUrlGrabberClassList.Populate(
  DefaultPropertiesList: TJvUrlGrabberDefaultPropertiesList);
var
  I: Integer;
begin
  DefaultPropertiesList.Clear;
  for I := 0 to Count - 1 do
    DefaultPropertiesList.Add(Items[I].GetDefaultPropertiesClass.Create(DefaultPropertiesList));
end;

procedure TJvUrlGrabberClassList.SetItem(Index: Integer;
  const AGrabberClass: TJvUrlGrabberClass);
begin
  inherited Items[Index] := AGrabberClass;
end;

//=== TJvUrlGrabberThread ====================================================

constructor TJvUrlGrabberThread.Create(Grabber: TJvUrlGrabber);
begin
  inherited Create(True);
  FGrabber := Grabber;
end;

procedure TJvUrlGrabberThread.Ended;
begin
  FGrabber.DoEnded;
end;

procedure TJvUrlGrabberThread.Error;
begin
  FGrabber.DoError(FErrorText);
end;

procedure TJvUrlGrabberThread.Progress;
begin
  FGrabber.DoProgress(FStatus);
end;

procedure TJvUrlGrabberThread.DoProgress;
begin
  Synchronize(Progress);
end;

procedure TJvUrlGrabberThread.ParseUrl(Value: string; Protocol: string;
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

//=== TJvUrlGrabberDefaultPropertiesCollection ===============================

constructor TJvUrlGrabberDefaultPropertiesList.Create(AOwner: TJvUrlListGrabber);
begin
  {$IFDEF COMPILER6_UP}
  inherited Create(AOwner);
  Name := 'DefaultProperties';
  {$ELSE}
  inherited Create;
  {$ENDIF COMPILER6_UP}
  FItems := TObjectList.Create(True);
  JvUrlGrabberClassList.Populate(Self);
end;

destructor TJvUrlGrabberDefaultPropertiesList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TJvUrlGrabberDefaultPropertiesList.Add(Item: TJvUrlGrabberDefaultProperties);
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

function TJvUrlGrabberDefaultPropertiesList.GetItems(
  Index: Integer): TJvUrlGrabberDefaultProperties;
begin
  Result := TJvUrlGrabberDefaultProperties(FItems[Index]);
end;

procedure TJvUrlGrabberDefaultPropertiesList.SetItems(Index: Integer;
  const Value: TJvUrlGrabberDefaultProperties);
begin
  FItems[Index] := Value;
end;

function TJvUrlGrabberDefaultPropertiesList.GetItemsNamed(
  Name: string): TJvUrlGrabberDefaultProperties;
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

constructor TJvUrlGrabberDefPropEdTrick.Create(
  GrabberDefaults: TJvUrlGrabberDefaultProperties);
begin
  FDefaultProperties := GrabberDefaults;
end;

//=== TJvUrlGrabberDefaultProperties =========================================

constructor TJvUrlGrabberDefaultProperties.Create(AOwner: TJvUrlGrabberDefaultPropertiesList);
begin
  {$IFDEF COMPILER6_UP}
  inherited Create(AOwner);
  Name := GetSupportedUrlName;
  {$ELSE}
  inherited Create;
  {$ENDIF}
  FEditorTrick := TJvUrlGrabberDefPropEdTrick.Create(Self);
  FFileName := 'output.txt';
  FAgent := 'JEDI-VCL';
end;

destructor TJvUrlGrabberDefaultProperties.Destroy;
begin
  FEditorTrick.Free;
  inherited Destroy;
end;

initialization

finalization
  FinalizeUnit(sUnitName);

end.

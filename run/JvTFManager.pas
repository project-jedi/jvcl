{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTFManager.PAS, released on 2003-08-01.

The Initial Developer of the Original Code is Unlimited Intelligence Limited.
Portions created by Unlimited Intelligence Limited are Copyright (C) 1999-2002 Unlimited Intelligence Limited.
All Rights Reserved.

Contributor(s):
Mike Kolter (original code)

Last Modified: 2003-08-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvTFManager;

interface

{$IFDEF COMPILER3}
uses Windows, Classes, Controls, SysUtils, Messages, Graphics, ExtCtrls,
  Printers, JvTFUtils
  {$IFDEF USEJVCL}, JvComponent{$ENDIF};
{$ELSE}
uses Windows, Classes, Controls, SysUtils, Messages, Graphics, ImgList,
  ExtCtrls, Printers, JvTFUtils
  {$IFDEF USEJVCL}, JvComponent{$ENDIF};
{$ENDIF}

Const
   CN_REQUESTREFRESH = $BD01;


Type
  // Redeclaration of this type.  It is used in JvTFMonths.TJvTFDrawDWTitleEvent.
  // If not redeclared here, Delphi complains of 'unknown type' because it
  // will not automatically bring in 'JvTFUtils' into the uses clause when
  // a TJvTFDrawDWTitleEvent prototype is created.
  TDayOfWeek = JvTFUtils.TDayOfWeek;

  EJvTFScheduleManagerError = class(Exception);

  TJvTFServNotifyCode = (sncDestroyAppt,
                         sncDestroySchedule,
                         sncLoadAppt,
                         sncSchedLoadAppt,
                         sncSchedUnloadAppt,
                         sncPostAppt,
                         sncDeleteAppt,
                         sncRequestSchedule,
                         sncReleaseSchedule,
                         sncConnectComponent,
                         sncDisconnectComponent,
                         sncConnectControl,
                         sncDisconnectControl,
                         sncConnectAppt,
                         sncDisconnectAppt,
                         sncRefresh);

  TJvTFScheduleManager = class;
  TJvTFSched = class;
  TJvTFAppt = class;
  TJvTFComponent = class;
  TJvTFControl = class;
  TJvTFPrinter = class;
  TJvTFHint = class;
//  TJvTFNavigator = class;

  TJvTFSchedClass = class of TJvTFSched;
  TJvTFApptClass = class of TJvTFAppt;
  TJvTFHintClass = class of TJvTFHint;

  TCNRequestRefresh = record
    Msg : Cardinal;
    Schedule : TJvTFSched;
    Unused : LongInt;
    Result : LongInt;
  End;

  TJvTFDateList = class
  private
    FOnChange : TNotifyEvent;
  protected
    FList : TStringList;
    function GetDate(Index: Integer) : TDate;
    procedure Change; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(aDate : TDate) : Integer;
    procedure Delete(Index : Integer);
    procedure Clear;
    function Count : Integer;
    function IndexOf(aDate : TDate) : Integer;
    property Dates[Index: Integer] : TDate read GetDate; default;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvTFNavEvent = procedure(Sender: TObject; aControl: TJvTFControl;
    SchedNames: TStringList; Dates: TJvTFDateList) of object;
  TJvTFControlEvent = procedure(Sender: TObject; aControl: TJvTFControl) of object;
  TJvTFSchedEvent = Procedure(Sender : TObject; Schedule : TJvTFSched) of object;
  TJvTFApptEvent = Procedure(Sender : TObject; Appt : TJvTFAppt) of object;
  TJvTFVarApptEvent = Procedure(Sender : TObject; var Appt : TJvTFAppt) of object;
  TJvTFFlushEvent = Procedure(Sender, FlushObj: TObject; var FlushIt: Boolean) of object;

  // implicit post fix
  TJvTFPostApptQueryEvent = procedure(Sender: TObject; Appt: TJvTFAppt;
    var CanPost: Boolean) of object;

  TJvTFCustomImageMap = class(TObject)
  private
    FMap : TStringList;
    function GetImage(MapIndex : Integer) : Integer;
    procedure SetImage(MapIndex : Integer; Value : Integer);
    function GetImageName(MapIndex : Integer) : String;
  protected
    FAppt : TJvTFAppt;
    procedure Change;
  public
    constructor Create(anAppt : TJvTFAppt);
    destructor Destroy; override;
    property Images[MapIndex : Integer] : Integer read GetImage write SetImage; default;
    property ImageNames[MapIndex : Integer] : String read GetImageName;
    function Count : Integer;
    procedure Add(ImageName : String; ImageIndex : Integer);
    procedure Delete(MapIndex : Integer);
    procedure Move(SrcMapIndex, DestMapIndex : Integer);
    function FindMapIndex(ImageName : String) : Integer;
    function FindImageIndex(ImageName : String) : Integer;
    procedure Clear;
    procedure Assign(Source: TJvTFCustomImageMap); dynamic;
  end;

  TJvTFStatePic = (spAlarmEnabled, spAlarmDisabled, spShared, spRecurring,
               spModified);

  TStateImageMap = class(TPersistent)
  private
    {$IFDEF COMPILER3}
    FPics : Array[Ord(Low(TJvTFStatePic))..Ord(High(TJvTFStatePic))] of Integer;
    {$ELSE}
    FPics : Array[Low(TJvTFStatePic)..High(TJvTFStatePic)] of Integer;
    {$ENDIF}

    {$IFDEF COMPILER3}
    procedure SetImage(Index, Value : Integer);
    function GetImage(Index : Integer) : Integer;
    {$ELSE}
    procedure SetImage(StatePicID : TJvTFStatePic; Value : Integer);
    function GetImage(StatePicID : TJvTFStatePic) : Integer;
    {$ENDIF}
  protected
    FScheduleManager : TJvTFScheduleManager;
    FUpdating : Boolean;
    procedure Change;
  public
    constructor Create(Serv : TJvTFScheduleManager);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    {$IFDEF COMPILER3}
    property Pics[Index : Integer] : Integer read GetImage write SetImage;
    {$ELSE}
    property Pics[Index : TJvTFStatePic] : Integer read GetImage write SetImage;
    {$ENDIF}
  published
    {$IFDEF COMPILER3}
    property AlarmEnabled: Integer index Ord(spAlarmEnabled) read GetImage write SetImage;
    property AlarmDisabled: Integer index Ord(spAlarmDisabled) read GetImage write SetImage;
    property Shared: Integer index Ord(spShared) read GetImage write SetImage;
    property Recurring: Integer index Ord(spRecurring) read GetImage write SetImage;
    property Modified: Integer index Ord(spModified) read GetImage write SetImage;
    {$ELSE}
    property AlarmEnabled: Integer index spAlarmEnabled read GetImage write SetImage;
    property AlarmDisabled: Integer index spAlarmDisabled read GetImage write SetImage;
    property Shared: Integer index spShared read GetImage write SetImage;
    property Recurring: Integer index spRecurring read GetImage write SetImage;
    property Modified: Integer index spModified read GetImage write SetImage;
    {$ENDIF}
  end;

  TJvTFTimeRange = record
    StartTime : TTime;
    EndTime : TTime;
  end;

  {$IFNDEF COMPILER3}
  TDynTimeRangeArray = Array of TJvTFTimeRange;
  {$ENDIF}

  {$IFNDEF COMPILER3}
  TDynApptArray = Array of TJvTFAppt;
  {$ENDIF}

  {$IFNDEF COMPILER3}
  TDynSchedArray = Array of TJvTFSched;
  {$ENDIF}

  TJvTFAppt = class(TObject)
  private
    FStartDate : TDate;
    FEndDate : TDate;
    FStartTime : TTime;
    FEndTime : TTime;
    FDescription : String;
    FAlarmEnabled : Boolean;
    FAlarmAdvance : Integer;
    FImageMap : TJvTFCustomImageMap;
    FData : Integer;
    FPersistent : Boolean;
    FColor : TColor;
    FBarColor : TColor;
    FRefreshed : Boolean;

    function GetDescription: String;
    procedure SetDescription(Value : String);
    procedure SetAlarmEnabled(Value : Boolean);
    procedure SetAlarmAdvance(Value : Integer);
    procedure SetColor(Value : TColor);
    procedure SetBarColor(Value : TColor);
    function GetStartDateTime : TDateTime;
    function GetEndDateTime : TDateTime;
    function GetStartDate : TDate;
    function GetEndDate : TDate;
    function GetStartTime : TTime;
    function GetEndTime : TTime;
    procedure SetRefreshed(Value: Boolean);
  protected
    FID : String;
    FModified : Boolean;
    FScheduleManager : TJvTFScheduleManager;
    FConnections : TStringList;
    FSchedules : TStringList;
    FDeleting : Boolean;
    // implicit post fix
    FUpdating : Boolean;

    procedure Notify(Sender : TObject; Code : TJvTFServNotifyCode);
    procedure NotifyManager(Serv : TJvTFScheduleManager; Sender : TObject;
      Code : TJvTFServNotifyCode);
    procedure NotifySchedule(Sched : TJvTFSched; Sender : TObject;
      Code : TJvTFServNotifyCode);

    function GetConnection(Index : Integer) : TJvTFSched;
    function GetSchedule(Index : Integer) : String;
    procedure CheckConnections;

    procedure Connect(Schedule : TJvTFSched);
    procedure Disconnect(Schedule : TJvTFSched);
    procedure Change;
    procedure InternalClearSchedules;
    procedure DeleteApptNotification;
    // implicit post fix
    procedure PostApptNotification;
    procedure RefreshNotification;
  public
    constructor Create(Serv : TJvTFScheduleManager; ApptID : String); virtual;
    destructor Destroy; override;

    procedure Assign(Source: TJvTFAppt); dynamic;
    procedure SetStartEnd(NewStartDate : TDate; NewStartTime : TTime;
      NewEndDate : TDate; NewEndTime : TTime);

    procedure SetModified;
    function Modified : Boolean; dynamic;
    property ScheduleManager : TJvTFScheduleManager read FScheduleManager;

    function ConnectionCount : Integer;
    property Connections[Index : Integer] : TJvTFSched read GetConnection;

    function ScheduleCount : Integer;
    property Schedules[Index : Integer] : String read GetSchedule;
    procedure AddSchedule(SchedName : String);
    procedure RemoveSchedule(SchedName : String);
    procedure AssignSchedules(List : TStrings);
    procedure ClearSchedules;
    function IndexOfSchedule(SchedName : String) : Integer;
    function Shared : Boolean;

    procedure Post;
    procedure Refresh;
    procedure Delete;

    // implicit post fix
    procedure BeginUpdate;
    procedure EndUpdate;
    property Updating : Boolean read FUpdating;

    property ImageMap : TJvTFCustomImageMap read FImageMap write FImageMap;
    procedure RefreshControls;
    property Refreshed : Boolean read FRefreshed write SetRefreshed;
  published
    property ID : String read FID;
    property StartDate : TDate read GetStartDate;
    property EndDate : TDate read GetEndDate;
    property StartTime : TTime read GetStartTime;
    property EndTime : TTime read GetEndTime;
    property StartDateTime : TDateTime read GetStartDateTime;
    property EndDateTime : TDateTime read GetEndDateTime;
    property Description : String read GetDescription write SetDescription;
    property AlarmEnabled : Boolean read FAlarmEnabled write SetAlarmEnabled;
    property AlarmAdvance : Integer read FAlarmAdvance write SetAlarmAdvance;
    property Data : Integer read FData write FData;
    property Persistent : Boolean read FPersistent write FPersistent;
    property Color : TColor read FColor write SetColor default clDefault;
    property BarColor : TColor read FBarColor write SetBarColor default clDefault;
  end;

  TJvTFSched = class(TObject)
  private
    FAppts : TStringList;
    FConControls : TStringList;
    FConComponents : TStringList;
    FDestroying : Boolean;
    FData : Integer;
    FPersistent : Boolean;
    FSchedDisplayName : String;
    procedure SetSchedDisplayName(Value : String);

    function GetAppt(Index : Integer) : TJvTFAppt;
  protected
    FSchedName : String;
    FSchedDate : TDate;
    FScheduleManager : TJvTFScheduleManager;
    FCached : Boolean;
    FCachedTime : DWORD;
    procedure Notify(Sender : TObject; Code : TJvTFServNotifyCode);
    procedure NotifyManager(Serv : TJvTFScheduleManager; Sender : TObject;
      Code : TJvTFServNotifyCode);
    procedure NotifyAppt(Appt : TJvTFAppt; Sender : TObject;
      Code : TJvTFServNotifyCode);
    function GetConControl(Index : Integer) : TJvTFControl;
    function GetConComponent(Index : Integer) : TJvTFComponent;
    procedure ConnectAppt(Appt : TJvTFAppt);
    procedure DisconnectAppt(Appt : TJvTFAppt);
    procedure ConnectionsOnChange(Sender: TObject);
    procedure CheckConnections;
    {$IFNDEF COMPILER3}
    function GetFreeUsedTime(FreeTime : Boolean) : TDynTimeRangeArray; dynamic;
    {$ENDIF}
  public
    constructor Create(Serv : TJvTFScheduleManager; aName : String; aDate : TDate); virtual;
    destructor Destroy; override;

    function ApptCount : Integer;
    function ApptByID(ID : String) : TJvTFAppt;
    property Appts[Index : Integer] : TJvTFAppt read GetAppt;

    function ConControlCount : Integer;
    property ConControls[Index : Integer] : TJvTFControl read GetConControl;

    function ConComponentCount : Integer;
    property ConComponents[Index : Integer] : TJvTFComponent read GetConComponent;

    procedure AddAppt(Appt : TJvTFAppt);
    procedure RemoveAppt(Appt : TJvTFAppt);

    //procedure RefreshAppts;
    procedure Refresh;
    procedure PostAppts;

    // Conflict and free time methods
    {$IFNDEF COMPILER3}
    function GetFreeTime : TDynTimeRangeArray; dynamic;
    function GetUsedTime : TDynTimeRangeArray; dynamic;
    function TimeIsFree(TimeRange : TJvTFTimeRange) : Boolean; overload; dynamic;
    function TimeIsFree(RangeStart, RangeEnd : TTime) : Boolean; overload; dynamic;
    // The ApptHasConflicts(anAppt : TJvTFAppt) method declared here checks
    //  ONLY THIS SCHEDULE!!
    function ApptHasConflicts(anAppt : TJvTFAppt) : Boolean; dynamic;
    function EnumConflicts(TimeRange : TJvTFTimeRange) : TDynApptArray;
      overload; dynamic;
    function EnumConflicts(RangeStart, RangeEnd : TTime) : TDynApptArray;
      overload; dynamic;
    // The following EnumConflicts(anAppt : TJvTFAppt) checks
    //  ONLY THIS SCHEDULE!!
    function EnumConflicts(anAppt : TJvTFAppt) : TDynApptArray;
      overload; dynamic;
    {$ENDIF}

    property Cached : Boolean read FCached;
    property CachedTime : DWORD read FCachedTime;
    property Destroying : Boolean read FDestroying;

    function GetFirstAppt : TJvTFAppt;
    function GetLastAppt : TJvTFAppt;
  published
    property SchedDisplayName : String read FSchedDisplayName
      write SetSchedDisplayName;
    property SchedName : String read FSchedName;
    property SchedDate : TDate read FSchedDate;
    property ScheduleManager : TJvTFScheduleManager read FScheduleManager;
    property Data : Integer read FData write FData;
    property Persistent : Boolean read FPersistent write FPersistent;
  end;

  TJvTFScheduleManagerCacheType = (ctNone, ctTimed, ctBuffer);
  TJvTFScheduleManagerCache = class(TPersistent)
  private
    FCacheType : TJvTFScheduleManagerCacheType;
    FTimedDelay : Integer;
    FBufferCount : Integer;
    FTimer : TTimer;
    procedure SetCacheType(Value: TJvTFScheduleManagerCacheType);
    procedure SetTimedDelay(Value: Integer);
    procedure SetBufferCount(Value: Integer);
  protected
    FScheduleManager : TJvTFScheduleManager;
    procedure FlushManager; virtual;
    procedure TimerOnTimer(Sender: TObject); virtual;
  public
    constructor Create(SchedManager : TJvTFScheduleManager);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CacheType : TJvTFScheduleManagerCacheType read FCacheType write SetCacheType
      default ctTimed;
    property TimedDelay : Integer read FTimedDelay write SetTimedDelay
      default 30000;
    property BufferCount : Integer read FBufferCount write SetBufferCount
      default 7;
  end;

  TJvTFSchedLoadMode = (slmOnDemand, slmBatch);
  TJvTFLoadBatchEvent = procedure(Sender: TObject; BatchName: String;
    BatchStartDate, BatchEndDate: TDate) of object;

  TJvTFGetApptDisplayTextEvent = procedure(Sender: TObject; Source: TComponent;
    Appt: TJvTFAppt; var DisplayText : String) of object;

  TJvTFApptDescEvent = procedure(Sender: TObject; Appt: TJvTFAppt;
    var Description: String) of object;

  TJvTFScheduleManager = class(TComponent)
  private
    FAlwaysPost : Boolean;
    FAppts : TStringList;
    FSchedules : TStringList;
    FConControls : TStringList;
    FConComponents : TStringList;
    FOnNeedAppts : TJvTFSchedEvent;
    FOnRefreshAppt : TJvTFApptEvent;
    FOnRefreshSched : TJvTFSchedEvent;
    FOnRefreshAll : TNotifyEvent;
    FOnDeleteAppt : TJvTFApptEvent;
    FOnPostAppt : TJvTFApptEvent;
    FOnFlush : TJvTFFlushEvent;
    FOnCreateAppt : TJvTFApptEvent;
    FOnCreateSchedule : TJvTFSchedEvent;
    FOnDestroyAppt : TJvTFApptEvent;
    FOnDestroySchedule : TJvTFSchedEvent;
    FOnGetApptDisplayText : TJvTFGetApptDisplayTextEvent;
    FOnGetApptDescription : TJvTFApptDescEvent;
    FOnSetApptDescription : TJvTFApptDescEvent;

    FSchedLoadMode : TJvTFSchedLoadMode;
    FOnLoadBatch : TJvTFLoadBatchEvent;
    FOnBatchesProcessed : TNotifyEvent;

    FRefreshAutoReconcile : Boolean;

    {$IFDEF COMPILER3}
    FStateImages : TImageList;
    FCustomImages : TImageList;
    {$ELSE}
    FStateImages : TCustomImageList;
    FCustomImages : TCustomImageList;
    {$ENDIF}
    FStateImageMap : TStateImageMap;
    FCache : TJvTFScheduleManagerCache;

    // implicit post fix
    FOnPostApptQuery : TJvTFPostApptQueryEvent;

    function GetAppt(Index : Integer) : TJvTFAppt;
    function GetSchedule(Index : Integer) : TJvTFSched;
    function GetConControl(Index : Integer) : TJvTFControl;
    function GetConComponent(Index : Integer) : TJvTFComponent;
    {$IFDEF COMPILER3}
    procedure SetStateImages(Value : TImageList);
    procedure SetCustomImages(Value : TImageList);
    {$ELSE}
    procedure SetStateImages(Value : TCustomImageList);
    procedure SetCustomImages(Value : TCustomImageList);
    {$ENDIF}
    procedure SetCache(Value: TJvTFScheduleManagerCache);

    procedure SeTJvTFSchedLoadMode(Value: TJvTFSchedLoadMode);
    procedure SetRefreshAutoReconcile(Value: Boolean);
  protected
    FLoadingAppts : Boolean;
    FRefreshing : Boolean;
    FImageChangeLink : TChangeLink;
    FFlushing : Boolean;
    FDestroying : Boolean;

    FSchedBatch : TStringList;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure ConnectControl(ApptCtrl : TJvTFControl);
    procedure DisconnectControl(ApptCtrl : TJvTFControl);
    procedure ConnectComponent(utfComp : TJvTFComponent);
    procedure DisconnectComponent(utfComp : TJvTFComponent);

    procedure Notify(Sender : TObject; Code : TJvTFServNotifyCode); virtual;
    procedure NotifyAppt(Appt : TJvTFAppt; Sender : TObject;
      Code : TJvTFServNotifyCode);
    procedure NotifySchedule(Sched : TJvTFSched; Sender : TObject;
      Code : TJvTFServNotifyCode);
    procedure NotifyApptCtrl(ApptCtrl : TJvTFControl; Sender : TObject;
      Code : TJvTFServNotifyCode);
    procedure NotifyutfComp(utfComp : TJvTFComponent; Sender : TObject;
      Code : TJvTFServNotifyCode);

    procedure RetrieveSchedule(SchedName : String; SchedDate : TDate;
      var Schedule : TJvTFSched; var LoadedNow : Boolean);

    procedure NeedAppts(Schedule : TJvTFSched); virtual;
    procedure AddAppt(Appt : TJvTFAppt);
    procedure RemoveAppt(Appt : TJvTFAppt);
    procedure RemoveSchedule(Sched : TJvTFSched);

    //procedure RefreshAppt(Appt : TJvTFAppt);
    procedure DeleteAppt(Appt : TJvTFAppt);
    procedure PostAppt(Appt : TJvTFAppt);

    // implicit post fix
    function QueryPostAppt(Appt : TJvTFAppt) : Boolean;

    procedure AddToBatch(aSched : TJvTFSched);
    procedure LoadBatch(BatchName : String; BatchStartDate,
      BatchEndDate : TDate); virtual;

    {$IFDEF COMPILER3}
    procedure RequestRefresh(ApptCtrl : TJvTFControl;
      Schedule : TJvTFSched); dynamic;
    procedure ComponentRequestRefresh(utfComp : TJvTFComponent;
      Schedule : TJvTFSched); dynamic;
    {$ELSE}
    procedure RequestRefresh(ApptCtrl : TJvTFControl;
      Schedule : TJvTFSched); overload; dynamic;
    procedure RequestRefresh(utfComp : TJvTFComponent;
      Schedule : TJvTFSched); overload; dynamic;
    {$ENDIF}

    procedure ImageListChange(Sender: TObject);
    procedure FlushAppts;
    function FlushObject(FlushObj : TObject) : Boolean;

    procedure DoCreateApptEvent(anAppt : TJvTFAppt); dynamic;
    procedure DoCreateScheduleEvent(aSchedule : TJvTFSched); dynamic;
    procedure DoDestroyApptEvent(anAppt : TJvTFAppt); dynamic;
    procedure DoDestroyScheduleEvent(aSchedule : TJvTFSched); dynamic;

    procedure SetApptDescription(Appt: TJvTFAppt; var Value: String); virtual;
    procedure GetApptDescription(Appt: TJvTFAppt; var Value: String); virtual;
  public
    class procedure Ver(var Major, Minor, Control : Word);
    class function VerStr : String;
    class function GetScheduleID(SchedName : String; SchedDate : TDate) : String;
    class function GenerateApptID : String; virtual;

    function GetSchedClass : TJvTFSchedClass; dynamic;
    function GetApptClass : TJvTFApptClass; dynamic;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function ApptCount : Integer;
    property Appts[Index : Integer] : TJvTFAppt read GetAppt;
    function FindAppt(ID : String) : TJvTFAppt;

    function ScheduleCount : Integer;
    property Schedules[Index : Integer] : TJvTFSched read GetSchedule;
    function FindSchedule(SchedName : String; SchedDate : TDate) : TJvTFSched;

    function ConControlCount : Integer;
    property ConControls[Index : Integer] : TJvTFControl read GetConControl;
    function ConComponentCount : Integer;
    property ConComponents[Index : Integer] : TJvTFComponent read GetConComponent;

    {$IFDEF COMPILER3}
    function RequestSchedule(ApptCtrl : TJvTFControl; SchedName : String;
      SchedDate : TDate) : TJvTFSched;
    function ComponentRequestSchedule(utfComp : TJvTFComponent;
      SchedName : String; SchedDate : TDate) : TJvTFSched;
    {$ELSE}
    function RequestSchedule(ApptCtrl : TJvTFControl; SchedName : String;
      SchedDate : TDate) : TJvTFSched; overload;
    function RequestSchedule(ApptCtrl : TJvTFControl; SchedName : String;
      SchedDate : TDate; var LoadedNow : Boolean) : TJvTFSched; overload;

    function RequestSchedule(utfComp : TJvTFComponent; SchedName : String;
      SchedDate : TDate) : TJvTFSched; overload;
    function RequestSchedule(utfComp : TJvTFComponent; SchedName : String;
      SchedDate : TDate; var LoadedNow : Boolean) : TJvTFSched; overload;
    {$ENDIF}

    {$IFDEF COMPILER3}
    procedure ReleaseSchedule(ApptCtrl : TJvTFControl; SchedName : String;
      SchedDate : TDate);
    procedure ComponentReleaseSchedule(utfComp : TJvTFComponent;
      SchedName : String; SchedDate : TDate);
    {$ELSE}
    procedure ReleaseSchedule(ApptCtrl : TJvTFControl; SchedName : String;
      SchedDate : TDate); overload;
    procedure ReleaseSchedule(utfComp : TJvTFComponent; SchedName : String;
      SchedDate : TDate); overload;
    {$ENDIF}

    procedure ProcessBatches;

    procedure RequestAppt(ID : String; var Appt : TJvTFAppt; var New : Boolean);

    property LoadingAppts : Boolean read FLoadingAppts;
    property Refreshing : Boolean read FRefreshing;

    procedure dbPostAppt(Appt : TJvTFAppt);
    procedure dbDeleteAppt(Appt : TJvTFAppt);
    procedure dbRefreshAppt(Appt : TJvTFAppt);
    procedure dbRefreshSched(Sched : TJvTFSched);
    procedure dbRefreshAll;
    procedure dbRefreshOrphans;
    function dbNewAppt(ID : String) : TJvTFAppt;

    procedure PostAppts;
    procedure RefreshAppts;
    procedure ReconcileRefresh(Scope: TObject);

    procedure RefreshConnections(Trigger : TObject); virtual;
    property Flushing : Boolean read FFlushing;
    {$IFDEF COMPILER3}
    procedure Flush(All: Boolean); virtual;
    {$ELSE}
    procedure Flush(All: Boolean = False); virtual;
    {$ENDIF}

    function GetApptDisplayText(aComponent: TComponent;
      Appt: TJvTFAppt) : String; virtual;
  published
    property AlwaysPost : Boolean read FAlwaysPost write FAlwaysPost default False;
    property OnNeedAppts : TJvTFSchedEvent read FOnNeedAppts write FOnNeedAppts;
    property OnRefreshAppt : TJvTFApptEvent read FOnRefreshAppt write FOnRefreshAppt;
    property OnRefreshSched : TJvTFSchedEvent read FOnRefreshSched
      write FOnRefreshSched;
    property OnRefreshAll : TNotifyEvent read FOnRefreshAll write FOnRefreshAll;
    property OnPostAppt : TJvTFApptEvent read FOnPostAppt write FOnPostAppt;
    property OnDeleteAppt : TJvTFApptEvent read FOnDeleteAppt write FOnDeleteAppt;
    {$IFDEF COMPILER3}
    property StateImages : TImageList read FStateImages write SetStateImages;
    property CustomImages : TImageList read FCustomImages write SetCustomImages;
    {$ELSE}
    property StateImages : TCustomImageList read FStateImages write SetStateImages;
    property CustomImages : TCustomImageList read FCustomImages write SetCustomImages;
    {$ENDIF}
    property StateImageMap : TStateImageMap read FStateImageMap write FStateImageMap;
    property Cache : TJvTFScheduleManagerCache read FCache write SetCache;
    // implicit post fix
    property OnPostApptQuery : TJvTFPostApptQueryEvent read FOnPostApptQuery
      write FOnPostApptQuery;
    property OnFlush : TJvTFFlushEvent read FOnFlush write FOnFlush;
    property OnCreateAppt : TJvTFApptEvent read FOnCreateAppt write FOnCreateAppt;
    property OnDestroyAppt : TJvTFApptEvent read FOnDestroyAppt write FOnDestroyAppt;
    property OnCreateSchedule : TJvTFSchedEvent read FOnCreateSchedule
      write FOnCreateSchedule;
    property OnDestroySchedule : TJvTFSchedEvent read FOnDestroySchedule
      write FOnDestroySchedule;
    property OnLoadBatch : TJvTFLoadBatchEvent read FOnLoadBatch write FOnLoadBatch;
    property OnBatchesProcessed : TNotifyEvent read FOnBatchesProcessed
      write FOnBatchesProcessed;
    property OnGetApptDisplayText : TJvTFGetApptDisplayTextEvent
      read FOnGetApptDisplayText write FOnGetApptDisplayText;
    property OnGetApptDescription : TJvTFApptDescEvent read FOnGetApptDescription
      write FOnGetApptDescription;
    property OnSetApptDescription : TJvTFApptDescEvent read FOnSetApptDescription
      write FOnSetApptDescription;

    property SchedLoadMode : TJvTFSchedLoadMode read FSchedLoadMode
      write SeTJvTFSchedLoadMode default slmOnDemand;
    property RefreshAutoReconcile : Boolean read FRefreshAutoReconcile
      write SetRefreshAutoReconcile default False;
  end;

  TJvTFHintProps = class(TPersistent)
  private
    FHintColor : TColor;
    FHintHidePause : Integer;
    FHintPause : Integer;
    procedure SetHintColor(Value : TColor);
    procedure SetHintHidePause(Value : Integer);
    procedure SetHintPause(Value : Integer);
  protected
    FutfControl : TJvTFControl;
    procedure Change; virtual;
  public
    constructor Create(AOwner : TJvTFControl);
    procedure Assign(Source : TPersistent); override;
  published
    property HintColor : TColor read FHintColor write SetHintColor
      default clDefault;
    property HintHidePause : Integer read FHintHidePause write SetHintHidePause
      default -1;
    property HintPause : Integer read FHintPause write SetHintPause
      default -1;
  end;

  TJvTFHintType = (shtAppt, shtStartEnd, shtCell, shtObj);

  TJvTFShowutfHintEvent = Procedure(Sender: TObject; HintType: TJvTFHintType;
    Ref: TObject; var HintRect: TRect; var HintText: String) of object;

  // NOTE:
  // The Pause property has the same meaning as the Application.HintPause
  // property.  The ShortPause property has the same meaning as the
  // Application.HintHidePause property.
  TJvTFHint = class(THintWindow)
  private
    FTimer : TTimer;
    FPause : Integer;
    FShortPause : Integer;
    FOnShowHint : TJvTFShowutfHintEvent;
    FRefProps : TJvTFHintProps;
    procedure SetPause(Value : Integer);
    procedure SetShortPause(Value : Integer);
  protected
    FApptCtrl : TJvTFControl;
    FOldAppt : TJvTFAppt;
    FOldObj : TObject;
    FShortTimer : Boolean;
    FHintRect : TRect;
    FHintText : String;
    FHintCell : TPoint;
    FHintType : TJvTFHintType;
    procedure TimerOnTimer(Sender: TObject); virtual;
    procedure PrepTimer(Short : Boolean);
    procedure SetHintText(StartDate, EndDate: TDate; StartTime, EndTime: TTime;
      Desc: String; ShowDatesTimes, ShowDesc: Boolean);
    procedure DoHint(Sustained: Boolean);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PropertyCheck; dynamic;
  public
    {$IFDEF COMPILER3}
    constructor Create(anApptCtrl : TJvTFControl);
    {$ELSE}
    constructor Create(anApptCtrl : TJvTFControl); reintroduce;
    {$ENDIF}
    destructor Destroy; override;
    procedure ActivateHint(Rect: TRect; const AHint: String); override;
    procedure ApptHint(Appt : TJvTFAppt; X, Y : Integer;
      ShowDatesTimes, ShowDesc, FormattedDesc: Boolean); virtual;
    procedure StartEndHint(StartDate, EndDate: TDate; StartTime, EndTime: TTime;
      X, Y: Integer; ShowDates: Boolean);
    procedure CellHint(Row, Col: Integer; HintText: String; CellRect: TRect);

    procedure MultiLineObjHint(Obj: TObject; X, Y: Integer; Hints: TStrings);

    procedure ReleaseHandle; virtual;
    // See above note on Pause and ShortPause properties
    property Pause : Integer read FPause write SetPause default 3000;
    property ShortPause : Integer read FShortPause write SetShortPause default 1500;
    property OnShowHint : TJvTFShowutfHintEvent read FOnShowHint write FOnShowHint;
    property HintType : TJvTFHintType read FHintType;
    property RefProps : TJvTFHintProps read FRefProps write FRefProps;
  end;

  TJvTFDragInfo = class
  private
    FApptCtrl : TJvTFControl;
    FSchedule : TJvTFSched;
    FAppt : TJvTFAppt;
    FShift : TShiftState;
  public
    property ApptCtrl : TJvTFControl read FApptCtrl write FApptCtrl;
    property Schedule : TJvTFSched read FSchedule write FSchedule;
    property Appt : TJvTFAppt read FAppt write FAppt;
    property Shift : TShiftState read FShift write FShift;
  end;

  {$IFDEF USEJVCL}
  TJvTFComponent = class(TJVComponent)
  {$ELSE}
  TJvTFComponent = class(TComponent)
  {$ENDIF}
  private
    FScheduleManager : TJvTFScheduleManager;
    FSchedules : TStringList;
    procedure SetManager(Value : TJvTFScheduleManager);
    function GetSchedule(Index : Integer) : TJvTFSched;
  protected
    FDateFormat : String;
    FTimeFormat : String;

    procedure UpdateDesigner;

    procedure SetDateFormat(Value : String); virtual;
    procedure SetTimeFormat(Value : String); virtual;
    procedure Notify(Sender : TObject; Code : TJvTFServNotifyCode); virtual;
    procedure ReqSchedNotification(Schedule : TJvTFSched); virtual;
    procedure RelSchedNotification(Schedule : TJvTFSched); virtual;
    procedure NotifyManager(Serv : TJvTFScheduleManager; Sender : TObject;
      Code : TJvTFServNotifyCode);
    procedure RefreshComponent; dynamic;
    property DateFormat : String read FDateFormat write SetDateFormat;
    property TimeFormat : String read FTimeFormat write SetTimeFormat;
    procedure DestroyApptNotification(anAppt: TJvTFAppt); virtual;
    procedure DestroySchedNotification(aSched : TJvTFSched); virtual;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function ScheduleCount : Integer;
    property Schedules[Index : Integer] : TJvTFSched read GetSchedule;
    function FindSchedule(SchedName : String; SchedDate : TDate) : TJvTFSched;
    function RetrieveSchedule(SchedName : String; SchedDate : TDate) : TJvTFSched;
    procedure ReleaseSchedule(SchedName : String; SchedDate : TDate); virtual;
    procedure ReleaseSchedules;
    procedure ProcessBatches;
  published
    property ScheduleManager : TJvTFScheduleManager read FScheduleManager write SetManager;
  end;

  {$IFDEF USEJVCL}
  TJvTFControl = class(TJvCustomControl)
  {$ELSE}
  TJvTFControl = class(TCustomControl)
  {$ENDIF}
  private
    FScheduleManager : TJvTFScheduleManager;
    FSchedules : TStringList;
//    FNavigator : TJvTFNavigator;
//    FOnNavigate : TJvTFNavEvent;
    procedure SetManager(Value : TJvTFScheduleManager);
    function GetSchedule(Index: Integer): TJvTFSched;
//    procedure SetNavigator(Value: TJvTFNavigator);
  protected
    FDateFormat : String;
    FTimeFormat : String;
    FDragInfo : TJvTFDragInfo;
    FShift : TShiftState;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDateFormat(Value : String); virtual;
    procedure SetTimeFormat(Value : String); virtual;
    procedure Notify(Sender : TObject; Code : TJvTFServNotifyCode); virtual;
    procedure ReqSchedNotification(Schedule : TJvTFSched); virtual;
    procedure RelSchedNotification(Schedule : TJvTFSched); virtual;
    procedure NotifyManager(Serv : TJvTFScheduleManager; Sender : TObject;
      Code : TJvTFServNotifyCode);
    procedure CNRequestRefresh(var Msg : TCNRequestRefresh); message CN_REQUESTREFRESH;
    procedure RefreshControl; dynamic;
    property DateFormat : String read FDateFormat write SetDateFormat;
    property TimeFormat : STring read FTimeFormat write SetTimeFormat;
    procedure DestroyApptNotification(anAppt: TJvTFAppt); virtual;
    procedure DestroySchedNotification(aSched: TJvTFSched); virtual;
    procedure DoStartDrag(var DragObject : TDragObject); override;
    procedure DoEndDrag(Target : TObject; X, Y : Integer); override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState;
                        X, Y : Integer); override;
    procedure Navigate(aControl: TJvTFControl; SchedNames: TStringList;
      Dates: TJvTFDateList); virtual;
//    property Navigator : TJvTFNavigator read FNavigator write SetNavigator;
//    property OnNavigate : TJvTFNavEvent read FOnNavigate write FOnNavigate;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function ScheduleCount : Integer;
    property Schedules[Index : Integer] : TJvTFSched read GetSchedule;
    function FindSchedule(SchedName : String; SchedDate : TDate) : TJvTFSched;
    function RetrieveSchedule(SchedName : String; SchedDate : TDate) : TJvTFSched;
    procedure ReleaseSchedule(SchedName : String; SchedDate : TDate); virtual;
    procedure ReleaseSchedules;
    property DragInfo : TJvTFDragInfo read FDragInfo;
    procedure ProcessBatches;
  published
    property ScheduleManager : TJvTFScheduleManager read FScheduleManager write SetManager;
  end;

  EJvTFPrinterError = class(Exception);
  TJvTFMargins = TRect;
  TJvTFPrinterMeasure = (pmPixels, pmInches, pmMM);
  TJvTFPrinterState = (spsNoDoc, spsCreating, spsAssembling, spsFinished);
  TJvTFPrinterDrawEvent = procedure(Sender : TObject; aCanvas : TCanvas;
    aRect : TRect; PageNum : Integer) of object;

  TJvTFProgressEvent = procedure(Sender: TObject; Current, Total: Integer)
    of object;

  TJvTFPrinterPageLayout = class(TPersistent)
  private
    FFooterHeight : Integer;
    FHeaderHeight : Integer;
    FMargins : TJvTFMargins;
    FutfPrinter : TJvTFPrinter;
    procedure SetFooterHeight(Value : Integer);
    procedure SetHeaderHeight(Value : Integer);
    function GetMargin(Index : Integer) : Integer;
    procedure SetMargin(Index : Integer; Value : Integer);
  protected
    procedure Change; virtual;
    property utfPrinter : TJvTFPrinter read FutfPrinter;
    procedure SetPropertyCheck;
  public
    constructor Create(autfPrinter : TJvTFPrinter); virtual;
    procedure Assign(Source : TPersistent); override;
  published
    property FooterHeight : Integer read FFooterHeight write SetFooterHeight;
    property HeaderHeight : Integer read FHeaderHeight write SetHeaderHeight;
    property MarginLeft   : Integer index 1 read GetMargin write SetMargin;
    property MarginTop    : Integer index 2 read GetMargin write SetMargin;
    property MarginRight  : Integer index 3 read GetMargin write SetMargin;
    property MarginBottom : Integer index 4 read GetMargin write SetMargin;
  end;

  TJvTFPrinter = class(TJvTFComponent)
  private
    FPages : TStringList;
    FBodies : TStringList;
    FMarginOffsets : TJvTFMargins; // always in pixels
    FMeasure : TJvTFPrinterMeasure;
    FOnDrawBody : TJvTFPrinterDrawEvent;
    FOnDrawHeader : TJvTFPrinterDrawEvent;
    FOnDrawFooter : TJvTFPrinterDrawEvent;
    FOnPrintProgress : TJvTFProgressEvent;
    FOnAssembleProgress : TJvTFProgressEvent;
    FOnMarginError : TNotifyEvent;
    FTitle : String;
    FDirectPrint : Boolean;

    function GetPage(Index : Integer) : TMetafile;
    function GetBodyHeight : Integer;  // always in pixels
    function GetBodyWidth : Integer;  // always in pixels
    function GetBodyLeft : Integer;  // always in pixels
    function GetBodyTop : Integer;  // always in pixels
    function GetDocDateTime : TDateTime;
    procedure SetPageLayout(Value : TJvTFPrinterPageLayout);
    procedure SetDirectPrint(Value : Boolean);
  protected
    FPageLayout : TJvTFPrinterPageLayout;
    FState : TJvTFPrinterState;
    FDocDateTime : TDateTime;
    FPageCount : Integer;  // NOTE: SEE GetPageCount !!
    FConvertingProps : Boolean;
    FAborted : Boolean;

    procedure SetMarginOffset(Index : Integer; Value : Integer); // always in pixels
    function GetMarginOffset(Index : Integer) : Integer; // always in pixels
    function GetUnprintable: TJvTFMargins; // always in pixels
    procedure MarginError; dynamic;
    procedure InitializeMargins;
    property BodyHeight : Integer read GetBodyHeight; // always in pixels
    property BodyWidth : Integer read GetBodyWidth; // always in pixels
    property BodyLeft : Integer read GetBodyLeft; // always in pixels
    property BodyTop : Integer read GetBodyTop; // always in pixels
    procedure DrawBody(aCanvas : TCanvas; aRect : TRect; PageNum : Integer); virtual;
    procedure DrawHeader(aCanvas : TCanvas; aRect : TRect; PageNum : Integer); virtual;
    procedure DrawFooter(aCanvas : TCanvas; aRect : TRect; PageNum : Integer); virtual;
    procedure SetTitle(Value : String); virtual;
    function GetPageCount : Integer;
    procedure SetMeasure(Value : TJvTFPrinterMeasure); virtual;
    procedure CreateLayout; virtual;
    procedure SetPropertyCheck; dynamic;

    procedure GetHeaderFooterRects(var HeaderRect, FooterRect : TRect);

    // document management methods
    procedure CreateDoc; dynamic;
    procedure NewPage; dynamic;
    procedure FinishDoc; dynamic;
    procedure NewDoc; dynamic;
    property DirectPrint : Boolean read FDirectPrint write SetDirectPrint
      default False;
  public
    class procedure Ver(var Major, Minor, Control : Word);
    class function VerStr : String;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property PageCount : Integer read GetPageCount;
    property Pages[Index : Integer] : TMetafile read GetPage;

    function ConvertMeasure(Value: Integer; FromMeasure,
      ToMeasure: TJvTFPrinterMeasure; Horizontal: Boolean): Integer;
    function ScreenToPrinter(Value : Integer; Horizontal : Boolean) : Integer;
    function PrinterToScreen(Value : Integer; Horizontal : Boolean) : Integer;

    property State : TJvTFPrinterState read FState;
    procedure FreeDoc; dynamic;
    procedure Print; dynamic;
    procedure AbortPrint;
    property DocDateTime : TDateTime read GetDocDateTime;
    property ConvertingProps : Boolean read FConvertingProps;
    procedure SaveDocToFiles(BaseFileName : TFileName);
    property Aborted : Boolean read FAborted;
  published
    property PageLayout : TJvTFPrinterPageLayout read FPageLayout
      write SetPageLayout;
    property Measure : TJvTFPrinterMeasure read FMeasure write SetMeasure
      default pmInches;
    property OnDrawBody : TJvTFPrinterDrawEvent read FOnDrawBody
      write FOnDrawBody;
    property OnDrawHeader : TJvTFPrinterDrawEvent read FOnDrawHeader
      write FOnDrawHeader;
    property OnDrawFooter : TJvTFPrinterDrawEvent read FOnDrawFooter
      write FOnDrawFooter;
    property OnPrintProgress : TJvTFProgressEvent read FOnPrintProgress
      write FOnPrintProgress;
    property OnAssembleProgress : TJvTFProgressEvent read FOnAssembleProgress
      write FOnAssembleProgress;
    property OnMarginError : TNotifyEvent read FOnMarginError
      write FOnMarginError;
    property Title : String read FTitle write SetTitle;
  end;

  TJvTFUniversalPrinter = class(TJvTFPrinter)
  public
    procedure NewDoc; override;
    procedure CreateDoc; override;
    procedure NewPage; override;
    procedure FinishDoc; override;
  published
    property DirectPrint;
  end;

  TJvTFDWNameSource = (dwnsSysLong, dwnsSysShort, dwnsCustom);

  TJvTFDrawDWTitleEvent = procedure(Sender: TObject; aCanvas: TCanvas;
    aRect: TRect; DOW: TDayOfWeek; DWName: String) of object;

  TJvTFDWNames = class(TPersistent)
  private
    FSource : TJvTFDWNameSource;
    FDWN_Sunday : String;
    FDWN_Monday : String;
    FDWN_Tuesday : String;
    FDWN_Wednesday : String;
    FDWN_Thursday : String;
    FDWN_Friday : String;
    FDWN_Saturday : String;

    FOnChange : TNotifyEvent;

    procedure SetDWN(Index: Integer; Value: String);
    function GetDWN(Index: Integer) : String;
    procedure SetSource(Value: TJvTFDWNameSource);
  protected
    procedure Change; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    function GetDWName(DWIndex : Integer) : String;
  published
    property Source : TJvTFDWNameSource read FSource write SetSource
      default dwnsSysShort;
    property DWN_Sunday    : String index 1 read GetDWN write SetDWN;
    property DWN_Monday    : String index 2 read GetDWN write SetDWN;
    property DWN_Tuesday   : String index 3 read GetDWN write SetDWN;
    property DWN_Wednesday : String index 4 read GetDWN write SetDWN;
    property DWN_Thursday  : String index 5 read GetDWN write SetDWN;
    property DWN_Friday    : String index 6 read GetDWN write SetDWN;
    property DWN_Saturday  : String index 7 read GetDWN write SetDWN;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

//  TJvTFNavigator = class(TComponent)
//  private
//    FBeforeNavigate : TJvTFNavEvent;
//    FAfterNavigate : TJvTFNavEvent;
//    FControls : TStringList;
//    function GetControl(Index: Integer): TJvTFControl;
//  protected
//    FNavigating : Boolean;
//    procedure RegisterControl(aControl: TJvTFControl);
//    procedure UnregisterControl(aControl: TJvTFControl);
//  public
//    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;
//
//    function ControlCount : Integer;
//    property Controls[Index: Integer] : TJvTFControl read GetControl;
//
//    procedure Navigate(aControl: TJvTFControl; SchedNames: TStringList;
//      Dates: TJvTFDateList); virtual;
//    property Navigating : Boolean read FNavigating;
//  published
//    property BeforeNavigate : TJvTFNavEvent read FBeforeNavigate
//      write FBeforeNavigate;
//    property AfterNavigate : TJvTFNavEvent read FAfterNavigate
//      write FAfterNavigate;
//  end;


implementation

Uses Dialogs, Forms;

{ Common }
function AdjustEndTime(aTime: TTime): TTime;
begin
  Result := Frac(Frac(aTime) - Frac(EncodeTime(0, 0, 1, 0)));
end;

Function CenterRect(Rect1, Rect2: TRect): TRect;
var
  Rect1Width,
  Rect1Height,
  Rect2Width,
  Rect2Height : Integer;
begin
  Rect1Width := Rect1.Right - Rect1.Left - 1;
  Rect1Height := Rect1.Bottom - Rect1.Top - 1;
  Rect2Width := Rect2.Right - Rect2.Left - 1;
  Rect2Height := Rect2.Bottom - Rect2.Top - 1;

  Result.Left := Rect1.Left + ((Rect1Width - Rect2Width) div 2) - 1;
  Result.Top := Rect1.Top + ((Rect1Height - Rect2Height) div 2) - 1;
  Result.Right := Result.Left + Rect2Width;
  Result.Bottom := Result.Top + Rect2Height;
end;

Function MoveRect(aRect : TRect; NewLeft, NewTop : Integer) : TRect;
Var
  XOffset,
  YOffset : Integer;
Begin
  XOffset := NewLeft - aRect.Left;
  YOffset := NewTop - aRect.Top;
  With Result do
    Begin
      Left := aRect.Left + XOffset;
      Right := aRect.Right + XOffset;
      Top := aRect.Top + YOffset;
      Bottom := aRect.Bottom + YOffset;
    End;
End;

Function StripCRLF(S : String) : String;
Var
  I : Integer;
Begin
  Result := '';
  For I := 1 to Length(S) do
    If (S[I] <> #13) and (S[I] <> #10) Then
      Result := Result + S[I];
End;


{ TJvTFCustomImageMap }
constructor TJvTFCustomImageMap.Create(anAppt: TJvTFAppt);
begin
  If not Assigned(anAppt) Then
    Raise EJvTFScheduleManagerError.Create('Could not create CustomImageMap.  ' +
                                      'Appointment not assigned');

  Inherited Create;
  FAppt := anAppt;
  FMap := TStringList.Create;
end;

destructor TJvTFCustomImageMap.Destroy;
begin
  FMap.Free;
  Inherited;
end;

function TJvTFCustomImageMap.GetImage(MapIndex: Integer): Integer;
begin
  Result := Integer(FMap.Objects[MapIndex]);
end;

procedure TJvTFCustomImageMap.SetImage(MapIndex, Value: Integer);
begin
  FMap.Objects[MapIndex] := TObject(Value);
end;

function TJvTFCustomImageMap.GetImageName(MapIndex : Integer): String;
begin
  Result := FMap[MapIndex];
end;

procedure TJvTFCustomImageMap.Change;
begin
  If Assigned(FAppt.ScheduleManager) Then
    Begin
      FAppt.ScheduleManager.RefreshConnections(FAppt);
      // implicit post fix
      FAppt.Change;
    End;
end;

function TJvTFCustomImageMap.Count: Integer;
begin
  Result := FMap.Count;
end;

procedure TJvTFCustomImageMap.Add(ImageName: String; ImageIndex: Integer);
begin
  If FMap.IndexOf(ImageName) = -1 Then
    Begin
      FMap.AddObject(ImageName, TObject(ImageIndex));
      Change;
    End;
end;

procedure TJvTFCustomImageMap.Delete(MapIndex: Integer);
begin
  FMap.Delete(MapIndex);
  Change;
end;

procedure TJvTFCustomImageMap.Move(SrcMapIndex, DestMapIndex: Integer);
begin
  FMap.Move(SrcMapIndex, DestMapIndex);
end;

function TJvTFCustomImageMap.FindMapIndex(ImageName: String): Integer;
begin
  Result := FMap.IndexOf(ImageName);
end;

function TJvTFCustomImageMap.FindImageIndex(ImageName: String): Integer;
begin
  Result := FindMapIndex(ImageName);
  If Result > -1 Then
    Result := GetImage(Result);
end;

procedure TJvTFCustomImageMap.Clear;
begin
  While FMap.Count > 0 do
    FMap.Delete(0);
  Change;
end;

procedure TJvTFCustomImageMap.Assign(Source: TJvTFCustomImageMap);
var
  I : Integer;
begin
  While FMap.Count > 0 do
    FMap.Delete(0);

  For I := 0 to Source.Count - 1 do
    Add(Source.ImageNames[I], Source.Images[I]);

  Change;
end;

{ TStateImageMap }
{$IFDEF COMPILER3}
constructor TStateImageMap.Create(Serv: TJvTFScheduleManager);
var
  I : Integer;
begin
  Inherited Create;

  For I := Ord(Low(TJvTFStatePic)) to Ord(High(TJvTFStatePic)) do
    FPics[I] := -1;

  FUpdating := False;
end;
{$ELSE}
constructor TStateImageMap.Create(Serv: TJvTFScheduleManager);
var
  I : TJvTFStatePic;
begin
  Inherited Create;

  For I := Low(TJvTFStatePic) to High(TJvTFStatePic) do
    FPics[I] := -1;

  FUpdating := False;
end;
{$ENDIF}

{$IFDEF COMPILER3}
procedure TStateImageMap.SetImage(Index, Value : Integer);
begin
  If Value < -1 Then
    Value := -1;
  If FPics[Index] <> Value Then
    Begin
      FPics[Index] := Value;
      Change;
    End;
end;
{$ELSE}
procedure TStateImageMap.SetImage(StatePicID: TJvTFStatePic; Value: Integer);
begin
  If Value < -1 Then
    Value := -1;
  If FPics[StatePicID] <> Value Then
    Begin
      FPics[StatePicID] := Value;
      Change;
    End;
end;
{$ENDIF}

{$IFDEF COMPILER3}
function TStateImageMap.GetImage(Index : Integer) : Integer;
begin
  Result := FPics[Index];
end;
{$ELSE}
function TStateImageMap.GetImage(StatePicID: TJvTFStatePic): Integer;
begin
  Result := FPics[StatePicID];
end;
{$ENDIF}

procedure TStateImageMap.Change;
begin
  If Assigned(FScheduleManager) and not (csLoading in FScheduleManager.ComponentState) and
     not (csDesigning in FScheduleManager.ComponentState) and not FUpdating Then
    FScheduleManager.RefreshConnections(nil);
end;

procedure TStateImageMap.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TStateImageMap.EndUpdate;
begin
  If FUpdating Then
    Begin
      FUpdating := False;
      Change;
    End;
end;

{$IFDEF COMPILER3}
procedure TStateImageMap.Clear;
var
  I : Integer;
begin
  For I := Ord(Low(TJvTFStatePic)) to Ord(High(TJvTFStatePic)) do
    FPics[I] := -1;
  Change;
end;
{$ELSE}
procedure TStateImageMap.Clear;
var
  I : TJvTFStatePic;
begin
  For I := Low(TJvTFStatePic) to High(TJvTFStatePic) do
    FPics[I] := -1;
  Change;
end;
{$ENDIF}

{$IFDEF COMPILER3}
procedure TStateImageMap.Assign(Source: TPersistent);
var
  Pic : Integer;
begin
  If Source is TStateImageMap Then
    Begin
      For Pic := Ord(Low(TJvTFStatePic)) to Ord(High(TJvTFStatePic)) do
        FPics[Pic] := TStateImageMap(Source).Pics[Pic];
      Change;
    End
  Else
    Inherited Assign(Source);
end;
{$ELSE}
procedure TStateImageMap.Assign(Source: TPersistent);
var
  Pic : TJvTFStatePic;
begin
  If Source is TStateImageMap Then
    Begin
      For Pic := Low(TJvTFStatePic) to High(TJvTFStatePic) do
        FPics[Pic] := TStateImageMap(Source).Pics[Pic];
      Change;
    End
  Else
    Inherited Assign(Source);
end;
{$ENDIF}

{ TJvTFAppt }
constructor TJvTFAppt.Create(Serv: TJvTFScheduleManager; ApptID: String);
begin
  If not Assigned(Serv) Then
    Raise EJvTFScheduleManagerError.Create('Could not create Appointment object.  ' +
                                      'ScheduleManager not assigned');

  Inherited Create;

  FSchedules := TStringList.Create;
  FConnections := TStringList.Create;

  FStartDate := Date;
  FStartTime := Time;
  FEndDate := Date;
  FEndTime := FStartTime + EncodeTime(0, 1, 0, 0);
  FScheduleManager := Serv;

  If ApptID <> '' Then
    FID := ApptID
  Else
    FID := FScheduleManager.GenerateApptID;

  FModified := False;
  FColor := clDefault;
  FBarColor := clDefault;

  FImageMap := TJvTFCustomImageMap.Create(Self);

  ScheduleManager.Notify(Self, sncLoadAppt);

  Serv.DoCreateApptEvent(Self);
end;

destructor TJvTFAppt.Destroy;
begin
  If Assigned(ScheduleManager) Then
    ScheduleManager.DoDestroyApptEvent(Self);

  ScheduleManager.Notify(Self, sncDestroyAppt);

  FSchedules.Free;
  FConnections.Free;
  FImageMap.Free;

  Inherited;
end;

function TJvTFAppt.GetDescription: String;
begin
  Result := FDescription;
  ScheduleManager.GetApptDescription(Self, Result);
end;

procedure TJvTFAppt.SetDescription(Value: String);
begin
  ScheduleManager.SetApptDescription(Self, Value);
  If Value <> FDescription Then
    Begin
      FDescription := Value;
      If not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
        Begin
          FModified := True;
          Change;
        End;
    End;
end;

procedure TJvTFAppt.SetAlarmEnabled(Value: Boolean);
begin
  If Value <> FAlarmEnabled Then
    Begin
      FAlarmEnabled := Value;
      If not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
        Begin
          FModified := True;
          Change;
        End;
    End;
end;

procedure TJvTFAppt.SetAlarmAdvance(Value: Integer);
begin
  If Value < 0 Then
    Value := 0;

  If Value <> FAlarmAdvance Then
    Begin
      FAlarmAdvance := Value;
      If not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
        Begin
          FModified := True;
          Change;
        End;
    End;
end;

procedure TJvTFAppt.SetColor(Value: TColor);
begin
  If Value <> FColor Then
    Begin
      FColor := Value;
      If not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
        Begin
          FModified := True;
          Change;
        End;
    End;
end;

procedure TJvTFAppt.SetBarColor(Value: TColor);
begin
  If Value <> FBarColor Then
    Begin
      FBarColor := Value;
      If not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
        Begin
          FModified := True;
          Change;
        End;
    End;
end;

procedure TJvTFAppt.Notify(Sender: TObject; Code: TJvTFServNotifyCode);
begin
  Case Code of
    sncConnectAppt     : Connect(TJvTFSched(Sender));
    sncDisconnectAppt  : Disconnect(TJvTFSched(Sender));
    // implicit post fix
    //sncPostAppt        : FModified := False;
    sncPostAppt        : PostApptNotification;
    sncDeleteAppt      : InternalClearSchedules;
    sncRefresh         : FModified := False;
  End;
end;

procedure TJvTFAppt.NotifyManager(Serv: TJvTFScheduleManager; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  If Assigned(Serv) Then
    Serv.Notify(Sender, Code)
  Else
    Raise EJvTFScheduleManagerError.Create('ScheduleManager notification failed.  ScheduleManager not assigned');
end;

procedure TJvTFAppt.NotifySchedule(Sched: TJvTFSched; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  If Assigned(Sched) Then
    Sched.Notify(Sender, Code)
  Else
    Raise EJvTFScheduleManagerError.Create('Schedule notification failed.  ' +
                                      'Schedule not assigned');
end;

function TJvTFAppt.GetConnection(Index: Integer): TJvTFSched;
begin
  Result := TJvTFSched(FConnections.Objects[Index]);
end;

function TJvTFAppt.GetSchedule(Index: Integer): String;
begin
  Result := FSchedules[Index];
end;

procedure TJvTFAppt.CheckConnections;
Var
  Schedule : TJvTFSched;
  I : Integer;
  aDate : TDate;
  Temp : TStringList;
begin
  // Schedules --> Connections
  For I := 0 to ScheduleCount - 1 do
    Begin
      aDate := StartDate;
      While Trunc(aDate) <= Trunc(EndDate) do
        Begin
          Schedule := ScheduleManager.FindSchedule(Schedules[I], aDate);
          If Assigned(Schedule) and (FConnections.IndexOfObject(Schedule) = -1) Then
            Connect(Schedule);

          aDate := aDate + 1;
        End;
    End;

  // Connections --> Schedules
  Temp := TStringList.Create;
  Try
    Temp.Assign(FConnections);
    For I := 0 to Temp.Count - 1 do
      Begin
        Schedule := TJvTFSched(Temp.Objects[I]);
        If (FSchedules.IndexOf(Schedule.SchedName) = -1) or
           ((Trunc(Schedule.SchedDate) < Trunc(StartDate)) or
            (Trunc(Schedule.SchedDate) > Trunc(EndDate))) Then
          Disconnect(Schedule);
      End;
  Finally
    Temp.Free;
  End;

  { implicit post fix
  If not FDeleting and not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
    // To avoid display anomolies we need to post the appt here.
    Post;
  }
end;

procedure TJvTFAppt.Connect(Schedule: TJvTFSched);
Var
  SchedID : String;
  I : Integer;
begin
  If Assigned(Schedule) Then
    Begin
      Schedule.Notify(Self, sncConnectAppt);

      SchedID := ScheduleManager.GetScheduleID(Schedule.SchedName, Schedule.SchedDate);
      I := FConnections.IndexOf(SchedID);
      If I = -1 Then
        Begin
          FConnections.AddObject(SchedID, Schedule);
          ScheduleManager.RefreshConnections(Schedule);
        End;
    End;
end;

procedure TJvTFAppt.Disconnect(Schedule: TJvTFSched);
Var
  I : Integer;
begin
  If Assigned(Schedule) Then
    Begin
      Schedule.Notify(Self, sncDisconnectAppt);

      I := FConnections.IndexOfObject(Schedule);
      If I > -1 Then
        Begin
          FConnections.Delete(I);
          ScheduleManager.RefreshConnections(Schedule);
        End;
    End;
end;

procedure TJvTFAppt.Change;
begin
  // implicit post fix
  If not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing and not Updating Then
    Post;
  ScheduleManager.RefreshConnections(Self);
end;

procedure TJvTFAppt.InternalClearSchedules;
begin
  FSchedules.Clear;
  CheckConnections;
end;

procedure TJvTFAppt.Assign(Source: TJvTFAppt);
var
  I : Integer;
begin
  For I := 0 to Source.ScheduleCount - 1 do
    AddSchedule(Source.Schedules[I]);
  ImageMap.Assign(Source.ImageMap);
  SetStartEnd(Source.StartDate, Source.StartTime, Source.EndDate, Source.EndTime);
  Description := Source.Description;
  AlarmEnabled := Source.AlarmEnabled;
  AlarmAdvance := Source.AlarmAdvance;
  Data := Source.Data;
end;

procedure TJvTFAppt.SetStartEnd(NewStartDate: TDate; NewStartTime: TTime;
  NewEndDate: TDate; NewEndTime: TTime);
begin
  // The following avoids time overflow into next day when it is not
  //  intended.  (Add appt to last row of days would cause invalid
  //  start/end exception.)
  If Frac(NewEndTime) <= EncodeTime(0, 0, 0, 999) Then
    NewEndTime := EncodeTime(23, 59, 59, 0);

  If Trunc(NewStartDate) <= Trunc(NewEndDate) Then
    Begin
      If Trunc(NewStartDate) = Trunc(NewEndDate) Then
        If Frac(NewStartTime) >= Frac(NewEndTime) Then
          Raise EJvTFScheduleManagerError.Create('Invalid start and end times');

      FStartDate := NewStartDate;
      FEndDate := NewEndDate;
      FStartTime := NewStartTime;
      FEndTime := NewEndTime;

      CheckConnections;

      If not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
        Begin
          FModified := True;
          Change;
        End
    End
  Else
    Raise EJvTFScheduleManagerError.Create('Invalid start and end dates');
end;

procedure TJvTFAppt.SetModified;
begin
  FModified := True;
  // implicit post fix
  Change;
end;

function TJvTFAppt.Modified: Boolean;
begin
  Result := FModified;
end;

function TJvTFAppt.ConnectionCount: Integer;
begin
  Result := FConnections.Count;
end;

function TJvTFAppt.ScheduleCount: Integer;
begin
  Result := FSchedules.Count;
end;

procedure TJvTFAppt.AddSchedule(SchedName: String);
Var
  aDate : TDate;
  Schedule : TJvTFSched;
begin
  If SchedName = '' Then
    Exit;

  // Add it to the schedules list
  If FSchedules.IndexOf(SchedName) = -1 Then
    Begin
      FSchedules.Add(SchedName);
      If not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
        Begin
          FModified := True;
          // implicit post fix
          Change;
        End;
    End;

  // Check for needed connections
  //  (Only connects to currently loaded schedules.  Will not load a schedule.)
  aDate := StartDate;
  While Trunc(aDate) <= Trunc(EndDate) do
    Begin
      Schedule := ScheduleManager.FindSchedule(SchedName, aDate);
      If Assigned(Schedule) Then
        Connect(Schedule);
      aDate := aDate + 1;
    End;

  { implicit post fix
  // To avoid display anomolies we need to post the appt here.
  If not FDeleting and not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
    Post;
  }
end;

procedure TJvTFAppt.RemoveSchedule(SchedName: String);
Var
  I : Integer;
  aDate : TDate;
  Schedule : TJvTFSched;
begin
  If SchedName = '' Then
    Exit;

  // Remove it from the schedule list
  I := FSchedules.IndexOf(SchedName);
  If I > -1 Then
    Begin
      FSchedules.Delete(I);
      If not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
        Begin
          FModified := True;
          // implicit post fix
          Change;
        End;
    End;

  // Check for invalid connections and disconnect
  aDate := StartDate;
  While Trunc(aDate) <= Trunc(EndDate) do
    Begin
      Schedule := ScheduleManager.FindSchedule(SchedName, aDate);
      If Assigned(Schedule) Then
        Disconnect(Schedule);

      aDate := aDate + 1;
    End;

  { implicit post fix
  // To avoid display anomolies we need to post the appt here.
  If not FDeleting and not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
    Post;
  }
end;

procedure TJvTFAppt.AssignSchedules(List: TStrings);
begin
  FSchedules.Assign(List);
  If not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
    Begin
      FModified := True;
      // implicit post fix
      Change;
    End;

  CheckConnections;
end;

procedure TJvTFAppt.ClearSchedules;
begin
  FSchedules.Clear;

  If not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
    Begin
      FModified := True;
      // implicit post fix
      Change;
    End;

  CheckConnections;
end;

function TJvTFAppt.IndexOfSchedule(SchedName: String): Integer;
begin
  Result := FSchedules.IndexOf(SchedName);
end;

function TJvTFAppt.Shared: Boolean;
begin
  Result := ScheduleCount > 1;
end;

procedure TJvTFAppt.Post;
begin
  ScheduleManager.dbPostAppt(Self);
end;

procedure TJvTFAppt.Refresh;
begin
  ScheduleManager.dbRefreshAppt(Self);
end;

procedure TJvTFAppt.Delete;
begin
  ScheduleManager.dbDeleteAppt(Self);
end;

procedure TJvTFAppt.RefreshControls;
begin
  ScheduleManager.RefreshConnections(Self);
end;


function TJvTFAppt.GetEndDateTime: TDateTime;
begin
  Result := Trunc(EndDate) + Frac(EndTime);
end;

function TJvTFAppt.GetStartDateTime: TDateTime;
begin
  Result := Trunc(StartDate) + Frac(StartTime);
end;

function TJvTFAppt.GetEndDate: TDate;
begin
  Result := Int(FEndDate);
end;

function TJvTFAppt.GetEndTime: TTime;
begin
  Result := Frac(FEndTime);
end;

function TJvTFAppt.GetStartDate: TDate;
begin
  Result := Int(FStartDate);
end;

function TJvTFAppt.GetStartTime: TTime;
begin
  Result := Frac(FStartTime);
end;

procedure TJvTFAppt.DeleteApptNotification;
begin
  FDeleting := True;
  Try
    InternalClearSchedules;
  Finally
    FDeleting := False;
  End;
end;

procedure TJvTFAppt.PostApptNotification;
begin
  FModified := False;
  FUpdating := False;
end;

procedure TJvTFAppt.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TJvTFAppt.EndUpdate;
begin
  If FUpdating Then
    Begin
      FUpdating := False;
      Change;
    End;
end;

procedure TJvTFAppt.SetRefreshed(Value: Boolean);
begin
  FRefreshed := Value;
end;

procedure TJvTFAppt.RefreshNotification;
begin
  FModified := False;
  Refreshed := False;
end;

{ TJvTFSched }
constructor TJvTFSched.Create(Serv : TJvTFScheduleManager; aName : String;
  aDate : TDate);
begin
  Inherited Create;

  FScheduleManager := Serv;
  FSchedName := aName;
  FSchedDate := aDate;

  FAppts := TStringList.Create;
  FConControls := TStringList.Create;
  FConControls.OnChange := ConnectionsOnChange;
  FConComponents := TStringList.Create;
  FConComponents.OnChange := ConnectionsOnChange;

  If Assigned(Serv) Then
    Serv.DoCreateScheduleEvent(Self);
end;

destructor TJvTFSched.Destroy;
Var
  Ctrl : TJvTFControl;
  Comp : TJvTFComponent;
  Appt : TJvTFAppt;
begin
  FDestroying := True;

  If Assigned(ScheduleManager) Then
    ScheduleManager.DoDestroyScheduleEvent(Self);

  While ConControlCount > 0 do
    Begin
      Ctrl := TJvTFControl(FConControls.Objects[0]);
      ScheduleManager.ReleaseSchedule(Ctrl, SchedName, SchedDate);
    End;

  While ConComponentCount > 0 do
    Begin
      Comp := TJvTFComponent(FConComponents.Objects[0]);
      {$IFDEF COMPILER3}
      ScheduleManager.ComponentReleaseSchedule(Comp, SchedName, SchedDate);
      {$ELSE}
      ScheduleManager.ReleaseSchedule(Comp, SchedName, SchedDate);
      {$ENDIF}
    End;

  While ApptCount > 0 do
    Begin
      Appt := Appts[0];
      Appt.Notify(Self, sncDisconnectAppt);
    End;

  ScheduleManager.Notify(Self, sncDestroySchedule);

  FAppts.Free;
  FConControls.Free;
  FConComponents.Free;

  Inherited;
end;

function TJvTFSched.GetAppt(Index: Integer): TJvTFAppt;
begin
  Result := TJvTFAppt(FAppts.Objects[Index]);
end;

procedure TJvTFSched.Notify(Sender: TObject; Code: TJvTFServNotifyCode);
Var
  I : Integer;
  ConList : TStringList;
begin
  If Sender is TJvTFControl Then
    ConList := FConControls
  Else If Sender is TJvTFComponent Then
    ConList := FConComponents
  Else
    ConList := nil;

  Case Code of
    sncRequestSchedule :
      If ConList.IndexOfObject(Sender) = -1 Then
        ConList.AddObject('', Sender);
    sncReleaseSchedule :
      Begin
        I := ConList.IndexOfObject(Sender);
        If I > -1 Then
          ConList.Delete(I);
      End;

    sncConnectAppt :
      ConnectAppt(TJvTFAppt(Sender));

    sncDisconnectAppt :
      DisconnectAppt(TJvTFAppt(Sender));
  End;
end;

procedure TJvTFSched.NotifyManager(Serv: TJvTFScheduleManager; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  If Assigned(Serv) Then
    Serv.Notify(Sender, Code)
  Else
    Raise EJvTFScheduleManagerError.Create('ScheduleManager notification failed.  ScheduleManager not assigned');
end;

procedure TJvTFSched.NotifyAppt(Appt: TJvTFAppt; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  If Assigned(Appt) Then
    Appt.Notify(Sender, Code)
  Else
    Raise EJvTFScheduleManagerError.Create('Appointment notification failed.  ' +
                                      'Appointment not assigned');
end;

function TJvTFSched.GetConControl(Index : Integer) : TJvTFControl;
begin
  Result := TJvTFControl(FConControls.Objects[Index]);
end;

function TJvTFSched.GetConComponent(Index : Integer) : TJvTFComponent;
begin
  Result := TJvTFComponent(FConComponents.Objects[Index]);
end;

procedure TJvTFSched.ConnectAppt(Appt: TJvTFAppt);
begin
  If FAppts.IndexOf(Appt.ID) = -1 Then
    FAppts.AddObject(Appt.ID, Appt);
end;

procedure TJvTFSched.DisconnectAppt(Appt: TJvTFAppt);
Var
  I : Integer;
begin
  I := FAppts.IndexOf(Appt.ID);
  If I > -1 Then
    FAppts.Delete(I);
end;

procedure TJvTFSched.ConnectionsOnChange(Sender: TObject);
begin
  If (FConControls.Count = 0) and (FConComponents.Count = 0) Then
    Begin
      FCached := True;
      FCachedTime := Windows.GetTickCount;
    End
  Else
    FCached := False;
end;

procedure TJvTFSched.CheckConnections;
var
  I : Integer;
  Appt : TJvTFAppt;
  DateHit,
  NameMatch,
  NotConnected : Boolean;
begin
  // Check each appt in the ScheduleManager to see if that appt should be connected
  //  to this schedule.  If so, then connect it.
  For I := 0 to ScheduleManager.ApptCount - 1 do
    Begin
      Appt := ScheduleManager.Appts[I];
      DateHit := (Trunc(SchedDate) >= Trunc(Appt.StartDate)) and
                 (Trunc(SchedDate) <= Trunc(Appt.EndDate));
      NameMatch := Appt.IndexOfSchedule(SchedName) > -1;
      NotConnected := ApptByID(Appt.ID) = nil;
      If DateHit and NameMatch and NotConnected Then
        Appt.Notify(Self, sncConnectAppt);
    End;
end;

{$IFNDEF COMPILER3}
function TJvTFSched.GetFreeUsedTime(FreeTime: Boolean): TDynTimeRangeArray;
var
  // 60 mins X 24 hrs = 1440 ==> minutes in a day
  DayArray : Array[0..1439] of Boolean;  // I'm a poet and don't know it.
  I,
  J,
  MinStart,
  MinEnd : Integer;
  anAppt : TJvTFAppt;
  StartTime,
  EndTime : TTime;
  Switch,
  MinIsFree,
  InRange : Boolean;

               ////////////////////////////////
               // SUBORDINATE ROUTINES
               ////////////////////////////////
               function TimeToMinNum(aTime : TTime) : Integer;
               var
                 H, M, S, MS : Word;
               begin
                 DecodeTime(aTime, H, M, S, MS);
                 Result := H * 60 + M;
               end;

               function MinNumToTime(MinNum : Integer) : TTime;
               begin
                 Result := EncodeTime(MinNum div 60, MinNum mod 60, 0, 0);
               end;

               procedure StartRange;
               begin
                StartTime := MinNumToTime(I);
                InRange := True;
               end;

               procedure EndRange;
               begin
                EndTime := MinNumToTime(I);

                // add range to resultant array
                SetLength(Result, Length(Result) + 1);
                Result[High(Result)].StartTime := StartTime;
                Result[High(Result)].EndTime := EndTime;

                InRange := False;
               end;

////////////////////
// MAIN ROUTINE
////////////////////
begin
  // Initialize resultant array
  SetLength(Result, 1);
  Result[0].StartTime := 0.0;
  Result[0].EndTime := EncodeTime(23, 59, 59, 0);

  // EXIT if nothing to do
  If ApptCount = 0 Then
    Begin
      If not FreeTime Then
        SetLength(Result, 0);
      Exit;
    End;

  // Initialize working array
  //  True ==> free minute
  //  False ==> used minute
  For I := 0 to 1439 do
    DayArray[I] := True;

  // Go through the appts and mark used minutes in the working array
  For I := 0 to ApptCount - 1 do
    Begin
      anAppt := Appts[I];
      MinStart := TimeToMinNum(anAppt.StartTime);
      MinEnd := TimeToMinNum(AdjustEndTime(anAppt.EndTime));

      For J := MinStart to MinEnd do
        DayArray[J] := False;
    End;

  // Now convert working array to resultant array
  SetLength(Result, 0);
  MinIsFree := not FreeTime;
  For I := 0 to 1439 do
    Begin
      Switch := DayArray[I] xor MinIsFree;
      MinIsFree := DayArray[I];
      If Switch Then
        If MinIsFree Then
          If FreeTime Then
            StartRange
          Else
            EndRange
        Else
          If FreeTime Then
            EndRange
          Else
            StartRange
    End;

  // close and add the last range if needed
  If InRange Then
    Begin
      I := 1439; // set I to last min of day
      EndRange;
    End;
end;
{$ENDIF}

function TJvTFSched.ApptCount: Integer;
begin
  Result := FAppts.Count;
end;

function TJvTFSched.ApptByID(ID: String): TJvTFAppt;
Var
  I : Integer;
begin
  Result := nil;
  I := FAppts.IndexOf(ID);
  If I > -1 Then
    Result := TJvTFAppt(FAppts.Objects[I]);
end;

function TJvTFSched.ConControlCount : Integer;
begin
  Result := FConControls.Count;
end;

function TJvTFSched.ConComponentCount : Integer;
begin
  Result := FConComponents.Count;
end;

procedure TJvTFSched.AddAppt(Appt: TJvTFAppt);
begin
  If Assigned(Appt) Then
    Appt.AddSchedule(SchedName);
end;

procedure TJvTFSched.RemoveAppt(Appt: TJvTFAppt);
begin
  If Assigned(Appt) Then
    Appt.RemoveSchedule(SchedName);
end;
{
procedure TJvTFSched.RefreshAppts;
Var
  I,
  J,
  K : Integer;
  ApptIDList,
  RefList : TStringList;
  Appt : TJvTFAppt;
  Sched : TJvTFSched;
  RefID : String;
begin
  // In a multi-user environment, appt objects may be deleted as a result
  // of calling dbRefreshAppt.  (Component user may call Appt.Free.)
  // To account for this we need to build a list of appt ID's instead of
  // working directly from the ScheduleManager's appointment list.
  // We also need to build a list of connections (utfComponents and
  // utfControls) that need to be refreshed.

  ApptIDList := TStringList.Create;
  RefList := TStringList.Create;
  RefList.Duplicates := dupIgnore;
  Try
    For I := 0 to ApptCount - 1 do
      Begin
        Appt := Appts[I];
        ApptIDList.Add(Appt.ID);
        For J := 0 to Appt.ConnectionCount - 1 do
          Begin
            Sched := Appt.Connections[J];
            For K := 0 to Sched.ConComponentCount - 1 do
              Begin
                RefID := IntToStr(Integer(Sched.ConComponents[K]));
                RefList.AddObject(RefID, Sched.ConComponents[K]);
              End;
            For K := 0 to Sched.ConControlCount - 1 do
              Begin
                RefID := IntToStr(Integer(Sched.ConControls[K]));
                RefList.AddObject(RefID, Sched.ConControls[K]);
              End;
          End;
      End;

    For I := 0 to ApptIDList.Count - 1 do
      Begin
        Appt := ScheduleManager.FindAppt(ApptIDList[I]);
        If Assigned(Appt) Then
          ScheduleManager.dbRefreshAppt(Appt);
      End;

    For I := 0 to RefList.Count - 1 do
      ScheduleManager.RefreshConnections(RefList.Objects[I]);
  Finally
    ApptIDList.Free;
    RefList.Free;
  End;
end;
}


procedure TJvTFSched.PostAppts;
Var
  I : Integer;
begin
  For I := 0 to ApptCount - 1 do
    ScheduleManager.dbPostAppt(Appts[I]);
end;

{$IFNDEF COMPILER3}
function TJvTFSched.GetFreeTime: TDynTimeRangeArray;
begin
  Result := GetFreeUsedTime(True);
end;
{$ENDIF}

{$IFNDEF COMPILER3}
function TJvTFSched.GetUsedTime: TDynTimeRangeArray;
begin
  Result := GetFreeUsedTime(False);
end;
{$ENDIF}

{$IFNDEF COMPILER3}
function TJvTFSched.TimeIsFree(TimeRange: TJvTFTimeRange): Boolean;
var
  Appt : TJvTFAppt;
  I : Integer;
begin
  Result := True;
  I := 0;

  While (I < ApptCount) and Result do
    Begin
      Appt := Appts[I];
      If (Frac(Appt.StartTime) <= Frac(AdjustEndTime(TimeRange.EndTime))) and
         (Frac(AdjustEndTime(Appt.EndTime))   >= Frac(TimeRange.StartTime)) Then
        Result := False
      Else
        Inc(I);
    End;
end;
{$ENDIF}

{$IFNDEF COMPILER3}
function TJvTFSched.TimeIsFree(RangeStart, RangeEnd: TTime): Boolean;
var
  TimeRange : TJvTFTimeRange;
begin
  TimeRange.StartTime := RangeStart;
  TimeRange.EndTime := RangeEnd;
  Result := TimeIsFree(TimeRange);
end;
{$ENDIF}

{$IFNDEF COMPILER3}
function TJvTFSched.ApptHasConflicts(anAppt: TJvTFAppt): Boolean;
var
  Appt : TJvTFAppt;
  I : Integer;
begin
  Result := False;
  I := 0;

  While (I < ApptCount) and not Result do
    Begin
      Appt := Appts[I];
      If (Appt <> anAppt) and // Don't flag for the given appt
         (Frac(Appt.StartTime) <= Frac(AdjustEndTime(anAppt.EndTime))) and
         (Frac(AdjustEndTime(Appt.EndTime))   >= Frac(anAppt.StartTime)) Then
        Result := True
      Else
        Inc(I);
    End;
end;
{$ENDIF}

{$IFNDEF COMPILER3}
function TJvTFSched.EnumConflicts(TimeRange: TJvTFTimeRange): TDynApptArray;
var
  Appt : TJvTFAppt;
  I : Integer;
begin
  SetLength(Result, 0);
  For I := 0 to ApptCount - 1 do
    Begin
      Appt := Appts[I];
      If (Frac(Appt.StartTime) <= Frac(AdjustEndTime(TimeRange.EndTime))) and
         (Frac(AdjustEndTime(Appt.EndTime))   >= Frac(TimeRange.StartTime)) Then
        Begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := Appt;
        End;
    End;
end;
{$ENDIF}

{$IFNDEF COMPILER3}
function TJvTFSched.EnumConflicts(RangeStart, RangeEnd: TTime): TDynApptArray;
var
  TimeRange : TJvTFTimeRange;
begin
  TimeRange.StartTime := RangeStart;
  TimeRange.EndTime := RangeEnd;
  Result := EnumConflicts(TimeRange);
end;
{$ENDIF}

{$IFNDEF COMPILER3}
function TJvTFSched.EnumConflicts(anAppt: TJvTFAppt): TDynApptArray;
var
  Appt : TJvTFAppt;
  I : Integer;
begin
  SetLength(Result, 0);
  For I := 0 to ApptCount - 1 do
    Begin
      Appt := Appts[I];
      If (Appt <> anAppt) and // don't add the given appt
         (Frac(Appt.StartTime) <= Frac(AdjustEndTime(anAppt.EndTime))) and
         (Frac(AdjustEndTime(Appt.EndTime))   >= Frac(anAppt.StartTime)) Then
        Begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := Appt;
        End;
    End;
end;
{$ENDIF}

function TJvTFSched.GetFirstAppt: TJvTFAppt;
var
  I : Integer;
  anAppt : TJvTFAppt;
begin
  Result := nil;
  I := 0;
  While (I < ApptCount) do
    Begin
      anAppt := Appts[I];
      If Trunc(anAppt.StartDate) < Trunc(SchedDate) Then
        Begin
          Result := anAppt;
          Break;  // APPOINTMENT STARTS AT 0:00 (12:00am) SO LEAVE LOOP
        End
      Else If not Assigned(Result) Then
        Result := anAppt
      Else
        If Frac(anAppt.StartTime) < Frac(Result.StartTime) Then
          Result := anAppt;
      Inc(I);
    End;
end;

function TJvTFSched.GetLastAppt: TJvTFAppt;
var
  I : Integer;
  anAppt : TJvTFAppt;
begin
  Result := nil;
  I := 0;
  While (I < ApptCount) do
    Begin
      anAppt := Appts[I];
      If Trunc(anAppt.EndDate) > Trunc(SchedDate) Then
        Begin
          Result := anAppt;
          Break;  // APPOINTMENT ENDS AT 23:59 (11:59pm) SO LEAVE LOOP
        End
      Else If not Assigned(Result) Then
        Result := anAppt
      Else
        If Frac(anAppt.EndTime) > Frac(Result.EndTime) Then
          Result := anAppt;
      Inc(I);
    End;
end;

procedure TJvTFSched.Refresh;
begin
  ScheduleManager.dbRefreshSched(Self);    
end;

procedure TJvTFSched.SetSchedDisplayName(Value: String);
begin
  If FSchedDisplayName <> Value Then
    Begin
      FSchedDisplayName := Value;
      ScheduleManager.RefreshConnections(Self);
    End;
end;

{ TJvTFScheduleManagerCache }
constructor TJvTFScheduleManagerCache.Create(SchedManager: TJvTFScheduleManager);
begin
  Inherited Create;
  FScheduleManager := SchedManager;

  FCacheType := ctTimed;
  FTimedDelay := 30000;
  FBufferCount := 7;

  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := TimerOnTimer;
  FTimer.Interval := FTimedDelay;
  FTimer.Enabled := FCacheType = ctTimed;
end;

destructor TJvTFScheduleManagerCache.Destroy;
begin
  FTimer.Free;
  Inherited;
end;

procedure TJvTFScheduleManagerCache.SetCacheType(Value: TJvTFScheduleManagerCacheType);
begin
  If Value <> FCacheType Then
    Begin
      FCacheType := Value;
      FTimer.Enabled := Value = ctTimed;
      FlushManager;
    End;
end;

procedure TJvTFScheduleManagerCache.SetTimedDelay(Value: Integer);
begin
  If Value < 0 Then
    Value := 0;
  If Value <> FTimedDelay Then
    Begin
      FTimedDelay := Value;
      FTimer.Enabled := False;
      FTimer.Interval := Value;
      If CacheType = ctTimed Then
        Begin
          FTimer.Enabled := True;
          FlushManager;
        End;
    End;
end;

procedure TJvTFScheduleManagerCache.SetBufferCount(Value: Integer);
begin
  If Value < 0 Then
    Value := 0;
  If Value <> FBufferCount Then
    Begin
      FBufferCount := Value;
      If CacheType = ctBuffer Then
        FlushManager;
    End;
end;

procedure TJvTFScheduleManagerCache.FlushManager;
begin
  If Assigned(FScheduleManager) Then
    FScheduleManager.Flush(False);
end;

procedure TJvTFScheduleManagerCache.TimerOnTimer(Sender: TObject);
begin
  FlushManager;
end;

procedure TJvTFScheduleManagerCache.Assign(Source: TPersistent);
begin
  If Source is TJvTFScheduleManagerCache Then
    Begin
      FCacheType := TJvTFScheduleManagerCache(Source).CacheType;
      FTimedDelay := TJvTFScheduleManagerCache(Source).TimedDelay;
      FBufferCount := TJvTFScheduleManagerCache(Source).BufferCount;
      If FTimer.Enabled Then
        Begin
          FTimer.Enabled := False;
          FTimer.Interval := FTimedDelay;
          FTimer.Enabled := FCacheType = ctTimed;
        End;
      FlushManager;
    End
  Else
    Inherited Assign(Source);
end;

{ TJvTFScheduleManager }
class procedure TJvTFScheduleManager.Ver(var Major, Minor, Control : Word);
begin
  // utf can be purchased piece-meal or as a suite.  If purchased
  // (or upgraded) piece-by-piece the possibility of componet version
  // conflicts arises.  This internal versioning system will be used
  // to combat that problem.

  // ** PLEASE DO NOT CHANGE THE NUMBERS BELOW !!
  Major := 1;
  Minor := 4;
  Control := 1;
end;

class function TJvTFScheduleManager.VerStr : String;
var
  Major,
  Minor,
  Control : Word;
begin
  Ver(Major, Minor, Control);
  Result := IntToStr(Major) + '.' + IntToStr(Minor) + '.' + IntToStr(Control);
end;


class function TJvTFScheduleManager.GetScheduleID(SchedName: String;
  SchedDate: TDate): String;
begin
  Result := SchedName + IntToStr(Trunc(SchedDate));
end;

class function TJvTFScheduleManager.GenerateApptID: String;
Var
  I : Integer;
begin
  Result := FloatToStr(Now);
  For I := 1 to 5 do
    Result := Result + Chr(Random(25) + 65);
end;

constructor TJvTFScheduleManager.Create(AOwner: TComponent);
begin
  Inherited;

  FSchedLoadMode := slmOnDemand;

  FAppts := TStringList.Create;
  FSchedules := TStringList.Create;

  FSchedBatch := TStringList.Create;
  FSchedBatch.Sorted := True;
  FSchedBatch.Duplicates := dupIgnore;

  FConControls := TStringList.Create;
  FConComponents := TStringList.Create;

  FStateImageMap := TStateImageMap.Create(Self);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  FCache := TJvTFScheduleManagerCache.Create(Self);
end;

destructor TJvTFScheduleManager.Destroy;
begin
  FDestroying := True;

  While ConControlCount > 0 do
    ConControls[0].ScheduleManager := nil;

  While ConComponentCount > 0 do
    ConComponents[0].ScheduleManager := nil;

  While ScheduleCount > 0 do
    Begin
      Schedules[0].Free;
    End;

  While ApptCount > 0 do
    Appts[0].Free;

  FAppts.Free;
  FSchedBatch.Free;
  FSchedules.Free;
  FConControls.Free;
  FConComponents.Free;
  FStateImageMap.Free;

  StateImages := nil;
  CustomImages := nil;
  FImageChangeLink.Free;

  FCache.Free;

  Inherited;
end;

function TJvTFScheduleManager.GetAppt(Index: Integer): TJvTFAppt;
begin
  Result := TJvTFAppt(FAppts.Objects[Index]);
end;

function TJvTFScheduleManager.GetSchedule(Index: Integer): TJvTFSched;
begin
  Result := TJvTFSched(FSchedules.Objects[Index]);
end;

function TJvTFScheduleManager.GetConControl(Index : Integer) : TJvTFControl;
begin
  Result := TJvTFControl(FConControls.Objects[Index]);
end;

function TJvTFScheduleManager.GetConComponent(Index : Integer) : TJvTFComponent;
begin
  Result := TJvTFComponent(FConComponents.Objects[Index]);
end;

{$IFDEF COMPILER3}
procedure TJvTFScheduleManager.SetStateImages(Value: TImageList);
begin
  If Assigned(FStateImages) Then
    FStateImages.UnregisterChanges(FImageChangeLink);

  FStateImages := Value;

  If Assigned(FStateImages) Then
    Begin
      FStateImages.RegisterChanges(FImageChangeLink);
      FStateImages.FreeNotification(Self);
    End;
end;
{$ELSE}
procedure TJvTFScheduleManager.SetStateImages(Value: TCustomImageList);
begin
  If Assigned(FStateImages) Then
    FStateImages.UnregisterChanges(FImageChangeLink);

  FStateImages := Value;

  If Assigned(FStateImages) Then
    Begin
      FStateImages.RegisterChanges(FImageChangeLink);
      FStateImages.FreeNotification(Self);
    End;
end;
{$ENDIF}

{$IFDEF COMPILER3}
procedure TJvTFScheduleManager.SetCustomImages(Value: TImageList);
begin
  If Assigned(FCustomImages) Then
    FCustomImages.UnregisterChanges(FImageChangeLink);

  FCustomImages := Value;

  If Assigned(FCustomImages) Then
    Begin
      FCustomImages.RegisterChanges(FImageChangeLink);
      FCustomImages.FreeNotification(Self);
    End;
end;
{$ELSE}
procedure TJvTFScheduleManager.SetCustomImages(Value: TCustomImageList);
begin
  If Assigned(FCustomImages) Then
    FCustomImages.UnregisterChanges(FImageChangeLink);

  FCustomImages := Value;

  If Assigned(FCustomImages) Then
    Begin
      FCustomImages.RegisterChanges(FImageChangeLink);
      FCustomImages.FreeNotification(Self);
    End;
end;
{$ENDIF}

procedure TJvTFScheduleManager.SetCache(Value: TJvTFScheduleManagerCache);
begin
  FCache.Assign(Value);
end;

procedure TJvTFScheduleManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  Inherited;
  If Operation = opRemove Then
    If AComponent = StateImages Then
      Begin
        StateImages := nil;
        RefreshConnections(nil);
      End
    Else If AComponent = CustomImages Then
      Begin
        CustomImages := nil;
        RefreshConnections(nil);
      End;
end;

procedure TJvTFScheduleManager.ConnectControl(ApptCtrl: TJvTFControl);
Var
  I : Integer;
begin
  If not Assigned(ApptCtrl) Then
    Exit;

  I := FConControls.IndexOfObject(ApptCtrl);
  If I = -1 Then
    FConControls.AddObject('', ApptCtrl);
end;

procedure TJvTFScheduleManager.DisconnectControl(ApptCtrl: TJvTFControl);
Var
  I : Integer;
begin
  If not Assigned(ApptCtrl) Then
    Exit;

  I := FConControls.IndexOfObject(ApptCtrl);
  If I > -1 Then
    Begin
      ApptCtrl.ReleaseSchedules;
      FConControls.Delete(I);
    End;
end;

procedure TJvTFScheduleManager.ConnectComponent(utfComp : TJvTFComponent);
var
  I : Integer;
begin
  If not Assigned(utfComp) Then
    Exit;

  I := FConComponents.IndexOfObject(utfComp);
  If I = -1 Then
    FConComponents.AddObject('', utfComp);
end;

procedure TJvTFScheduleManager.DisconnectComponent(utfComp : TJvTFComponent);
var
  I : Integer;
begin
  If not Assigned(utfComp) Then
    Exit;

  I := FConComponents.IndexOfObject(utfComp);
  If I > -1 Then
    Begin
      utfComp.ReleaseSchedules;
      FConComponents.Delete(I);
    End;
end;

procedure TJvTFScheduleManager.Notify(Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  Case Code of
    sncConnectComponent    : ConnectComponent(TJvTFComponent(Sender));
    sncDisconnectComponent : DisconnectComponent(TJvTFComponent(Sender));
    sncConnectControl      : ConnectControl(TJvTFControl(Sender));
    sncDisconnectControl   : DisconnectControl(TJvTFControl(Sender));
    sncLoadAppt            : AddAppt(TJvTFAppt(Sender));
    sncDestroyAppt         : RemoveAppt(TJvTFAppt(Sender));
    sncDestroySchedule     : RemoveSchedule(TJvTFSched(Sender));
  End;
end;

procedure TJvTFScheduleManager.NotifyAppt(Appt: TJvTFAppt; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  If Assigned(Appt) Then
    Appt.Notify(Sender, Code);
end;

procedure TJvTFScheduleManager.NotifySchedule(Sched: TJvTFSched; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  If Assigned(Sched) Then
    Sched.Notify(Sender, Code);
end;

procedure TJvTFScheduleManager.NotifyApptCtrl(ApptCtrl: TJvTFControl;
  Sender: TObject; Code: TJvTFServNotifyCode);
begin
  If Assigned(ApptCtrl) Then
    ApptCtrl.Notify(Sender, Code);
end;

procedure TJvTFScheduleManager.NotifyutfComp(utfComp : TJvTFComponent;
  Sender : TObject; Code : TJvTFServNotifyCode);
begin
  If Assigned(utfComp) Then
    utfComp.Notify(Sender, Code);
end;

procedure TJvTFScheduleManager.RetrieveSchedule(SchedName : String; SchedDate : TDate;
  var Schedule : TJvTFSched; var LoadedNow : Boolean);
Var
  SchedID : String;
  I : Integer;
begin
  SchedID := GetScheduleID(SchedName, SchedDate);
  I := FSchedules.IndexOf(SchedID);

  If I > -1 Then
    Begin
      Schedule := TJvTFSched(FSchedules.Objects[I]);
      LoadedNow := False;
    End
  Else
    Begin
      //Schedule := TJvTFSched.Create(Self, SchedName, SchedDate);
      Schedule := GetSchedClass.Create(Self, SchedName, SchedDate);
      FSchedules.AddObject(SchedID, Schedule);
      LoadedNow := True;
      If Cache.CacheType = ctBuffer Then
        Flush(False);
      Schedule.CheckConnections;
    End;
end;


procedure TJvTFScheduleManager.NeedAppts(Schedule : TJvTFSched);
begin
  FLoadingAppts := True;
  Try
    If Assigned(FOnNeedAppts) Then
      FOnNeedAppts(Self, Schedule);
  Finally
    FLoadingAppts := False;
    RefreshConnections(Schedule);
  End;
end;

procedure TJvTFScheduleManager.AddAppt(Appt: TJvTFAppt);
begin
  If FAppts.IndexOfObject(Appt) = -1 Then
    FAppts.AddObject(Appt.ID, Appt);
end;

procedure TJvTFScheduleManager.RemoveAppt(Appt: TJvTFAppt);
var
  I : Integer;
begin
  For I := 0 to ConControlCount - 1 do
    NotifyApptCtrl(ConControls[I], Appt, sncDestroyAppt);

  For I := 0 to ConComponentCount - 1 do
    NotifyutfComp(ConComponents[I], Appt, sncDestroyAppt);

  While Appt.ConnectionCount > 0 do
    Appt.Notify(Appt.Connections[0], sncDisconnectAppt);

  FAppts.Delete(FAppts.IndexOfObject(Appt));
end;

procedure TJvTFScheduleManager.RemoveSchedule(Sched: TJvTFSched);
var
  I : Integer;
begin
  For I := 0 to ConControlCount - 1 do
    NotifyApptCtrl(ConControls[I], Sched, sncDestroySchedule);

  For I := 0 to ConComponentCount - 1 do
    NotifyutfComp(ConComponents[I], Sched, sncDestroySchedule);

  FSchedules.Delete(FSchedules.IndexOfObject(Sched));
  Flush(False);
end;

{
procedure TJvTFScheduleManager.RefreshAppt(Appt: TJvTFAppt);
begin
  FLoadingAppts := True;
  Try
    NotifyAppt(Appt, Self, sncRefresh);
    If Assigned(FOnRefreshAppt) Then
      FOnRefreshAppt(Self, Appt);
  Finally
    FLoadingAppts := False;
  End;
end;
}

procedure TJvTFScheduleManager.DeleteAppt(Appt: TJvTFAppt);
begin
  If Assigned(FOnDeleteAppt) Then
    FOnDeleteAppt(Self, Appt);
end;

procedure TJvTFScheduleManager.PostAppt(Appt: TJvTFAppt);
begin
  If Assigned(FOnPostAppt) Then
    FOnPostAppt(Self, Appt);
end;


{$IFDEF COMPILER3}
procedure TJvTFScheduleManager.ComponentRequestRefresh(utfComp : TJvTFComponent;
  Schedule : TJvTFSched);
begin
  NotifyutfComp(utfComp, Self, sncRefresh);
end;
{$ENDIF}

procedure TJvTFScheduleManager.RequestRefresh(ApptCtrl: TJvTFControl;
  Schedule: TJvTFSched);
begin
  NotifyApptCtrl(ApptCtrl, Self, sncRefresh);
{
  If Assigned(ApptCtrl) Then
    Windows.PostMessage(ApptCtrl.Handle, CN_REQUESTREFRESH, Integer(Schedule), 0)
  Else
    Raise EJvTFScheduleManagerError.Create('Could not send refresh request.  ' +
                                      'ApptCtrl not assigned');
}
end;

{$IFNDEF COMPILER3}
procedure TJvTFScheduleManager.RequestRefresh(utfComp : TJvTFComponent;
  Schedule : TJvTFSched);
begin
  NotifyutfComp(utfComp, Self, sncRefresh);
end;
{$ENDIF}

procedure TJvTFScheduleManager.ImageListChange(Sender: TObject);
begin
  If not (csDestroying in ComponentState) Then
    RefreshConnections(nil);
end;

procedure TJvTFScheduleManager.FlushAppts;
var
  I : Integer;
begin
  I := 0;
  While I < ApptCount do
    If (Appts[I].ConnectionCount = 0) and not Appts[I].Persistent Then
      Begin
        If not FlushObject(Appts[I]) Then
          Inc(I);
      End
    Else
      Inc(I);
end;

function TJvTFScheduleManager.FlushObject(FlushObj: TObject) : Boolean;
var
  FlushIt : Boolean;
begin
  Result := False;
  If Assigned(FlushObj) Then
    Begin
      FlushIt := True;
      If Assigned(FOnFlush) Then
        FOnFlush(Self, FlushObj, FlushIt);
      If FlushIt Then
        FlushObj.Free;
      Result := FlushIt;
    End;
end;

procedure TJvTFScheduleManager.DoCreateApptEvent(anAppt : TJvTFAppt);
begin
  If Assigned(FOnCreateAppt) Then
    FOnCreateAppt(Self, anAppt);
end;

procedure TJvTFScheduleManager.DoCreateScheduleEvent(aSchedule : TJvTFSched);
begin
  If Assigned(FOnCreateSchedule) Then
    FOnCreateSchedule(Self, aSchedule);
end;

procedure TJvTFScheduleManager.DoDestroyApptEvent(anAppt : TJvTFAppt);
begin
  If Assigned(FOnDestroyAppt) Then
    FOnDestroyAppt(Self, anAppt);
end;

procedure TJvTFScheduleManager.DoDestroyScheduleEvent(aSchedule : TJvTFSched);
begin
  If Assigned(FOnDestroySchedule) Then
    FOnDestroySchedule(Self, aSchedule);
end;

function TJvTFScheduleManager.ApptCount: Integer;
begin
  Result := FAppts.Count;
end;

function TJvTFScheduleManager.FindAppt(ID: String): TJvTFAppt;
Var
  I : Integer;
begin
  Result := nil;
  I := FAppts.IndexOf(ID);
  If I > -1 Then
    Result := TJvTFAppt(FAppts.Objects[I]);
end;

function TJvTFScheduleManager.ScheduleCount: Integer;
begin
  Result := FSchedules.Count;
end;

function TJvTFScheduleManager.FindSchedule(SchedName: String;
  SchedDate: TDate): TJvTFSched;
Var
  I : Integer;
begin
  Result := nil;
  I := FSchedules.IndexOf(GetScheduleID(SchedName, SchedDate));
  If I > -1 Then
    Result := TJvTFSched(FSchedules.Objects[I]);
end;

function TJvTFScheduleManager.ConControlCount : Integer;
begin
  Result := FConControls.Count;
end;

function TJvTFScheduleManager.ConComponentCount : Integer;
begin
  Result := FConComponents.Count;
end;

{$IFDEF COMPILER3}
function TJvTFScheduleManager.ComponentRequestSchedule(utfComp : TJvTFComponent;
  SchedName : String; SchedDate : TDate) : TJvTFSched;
Var
  ApptsNeeded : Boolean;
begin
  RetrieveSchedule(SchedName, SchedDate, Result, ApptsNeeded);

  If Assigned(utfComp) Then
    Begin
      Result.Notify(utfComp, sncRequestSchedule);
      utfComp.Notify(Result, sncRequestSchedule);
    End;

  If ApptsNeeded Then
    NeedAppts(Result);
end;
{$ENDIF}


function TJvTFScheduleManager.RequestSchedule(ApptCtrl : TJvTFControl;
  SchedName: String; SchedDate: TDate): TJvTFSched;
Var
  ApptsNeeded : Boolean;
begin
  RetrieveSchedule(SchedName, SchedDate, Result, ApptsNeeded);

  If Assigned(ApptCtrl) Then
    Begin
      Result.Notify(ApptCtrl, sncRequestSchedule);
      ApptCtrl.Notify(Result, sncRequestSchedule);
    End;

  If ApptsNeeded Then
    If SchedLoadMode = slmOnDemand Then
      NeedAppts(Result)
    Else
      Begin
        AddToBatch(Result);
      End;
end;

{$IFNDEF COMPILER3}
function TJvTFScheduleManager.RequestSchedule(ApptCtrl : TJvTFControl;
  SchedName: String; SchedDate: TDate; var LoadedNow : Boolean): TJvTFSched;
begin
  RetrieveSchedule(SchedName, SchedDate, Result, LoadedNow);

  If Assigned(ApptCtrl) Then
    Begin
      Result.Notify(ApptCtrl, sncRequestSchedule);
      ApptCtrl.Notify(Result, sncRequestSchedule);
    End;

  If LoadedNow Then
    NeedAppts(Result);
end;
{$ENDIF}

{$IFNDEF COMPILER3}
function TJvTFScheduleManager.RequestSchedule(utfComp : TJvTFComponent;
  SchedName: String; SchedDate: TDate): TJvTFSched;
Var
  ApptsNeeded : Boolean;
begin
  RetrieveSchedule(SchedName, SchedDate, Result, ApptsNeeded);

  If Assigned(utfComp) Then
    Begin
      Result.Notify(utfComp, sncRequestSchedule);
      utfComp.Notify(Result, sncRequestSchedule);
    End;

  If ApptsNeeded Then
    NeedAppts(Result);
end;
{$ENDIF}

{$IFNDEF COMPILER3}
function TJvTFScheduleManager.RequestSchedule(utfComp : TJvTFComponent;
  SchedName : String; SchedDate : TDate; var LoadedNow : Boolean) : TJvTFSched;
begin
  RetrieveSchedule(SchedName, SchedDate, Result, LoadedNow);

  If Assigned(utfComp) Then
    Begin
      Result.Notify(utfComp, sncRequestSchedule);
      utfComp.Notify(Result, sncRequestSchedule);
    End;

  If LoadedNow Then
    NeedAppts(Result);
end;
{$ENDIF}

{$IFDEF COMPILER3}
procedure TJvTFScheduleManager.ComponentReleaseSchedule(utfComp : TJvTFComponent;
  SchedName : String; SchedDate : TDate);
Var
  SchedID : String;
  I : Integer;
  Schedule : TJvTFSched;
begin
  SchedID := GetScheduleID(SchedName, SchedDate);
  I := FSchedules.IndexOf(SchedID);

  If I > -1 Then
    Begin
      Schedule := TJvTFSched(FSchedules.Objects[I]);

      If Assigned(utfComp) Then
        Begin
          Schedule.Notify(utfComp, sncReleaseSchedule);
          utfComp.Notify(Schedule, sncReleaseSchedule);
        End;

      If Cache.CacheType = ctBuffer Then
        Flush(False);
    End;
end;
{$ENDIF}

procedure TJvTFScheduleManager.ReleaseSchedule(ApptCtrl : TJvTFControl;
  SchedName: String; SchedDate: TDate);
Var
  SchedID : String;
  I : Integer;
  Schedule : TJvTFSched;
begin
  SchedID := GetScheduleID(SchedName, SchedDate);
  I := FSchedules.IndexOf(SchedID);

  If I > -1 Then
    Begin
      Schedule := TJvTFSched(FSchedules.Objects[I]);

      If Assigned(ApptCtrl) Then
        Begin
          Schedule.Notify(ApptCtrl, sncReleaseSchedule);
          ApptCtrl.Notify(Schedule, sncReleaseSchedule);
        End;

      If (Cache.CacheType = ctBuffer) Then
        Flush(False);
    End;
end;

{$IFNDEF COMPILER3}
procedure TJvTFScheduleManager.ReleaseSchedule(utfComp : TJvTFComponent;
  SchedName : String; SchedDate : TDate);
Var
  SchedID : String;
  I : Integer;
  Schedule : TJvTFSched;
begin
  SchedID := GetScheduleID(SchedName, SchedDate);
  I := FSchedules.IndexOf(SchedID);

  If I > -1 Then
    Begin
      Schedule := TJvTFSched(FSchedules.Objects[I]);

      If Assigned(utfComp) Then
        Begin
          Schedule.Notify(utfComp, sncReleaseSchedule);
          utfComp.Notify(Schedule, sncReleaseSchedule);
        End;

      If Cache.CacheType = ctBuffer Then
        Flush(False);
    End;
end;
{$ENDIF}

procedure TJvTFScheduleManager.RequestAppt(ID: String; var Appt: TJvTFAppt;
  var New: Boolean);
Var
  I : Integer;
begin
  I := -1;
  If ID <> '' Then
    I := FAppts.IndexOf(ID);

  If I > -1 Then
    Begin
      Appt := TJvTFAppt(FAppts.Objects[I]);
      New := False;
    End
  Else
    Begin
      //Appt := TJvTFAppt.Create(Self, ID);
      Appt := GetApptClass.Create(Self, ID);
      New := True;
    End;
end;

procedure TJvTFScheduleManager.dbPostAppt(Appt: TJvTFAppt);
begin
{ implicit post fix
  If Assigned(Appt) Then
    If (AlwaysPost or Appt.Modified) Then
      Begin
        PostAppt(Appt);
        Appt.Notify(Self, sncPostAppt);
      End;
}

  // implicit post fix
  If Assigned(Appt) and
     (AlwaysPost or Appt.Modified) and
     QueryPostAppt(Appt) Then
    Begin
      PostAppt(Appt);
      Appt.Notify(Self, sncPostAppt);
    End;
end;

procedure TJvTFScheduleManager.dbDeleteAppt(Appt: TJvTFAppt);
begin
  If Assigned(Appt) Then
    Begin
      DeleteAppt(Appt);
      Appt.Notify(Self, sncDeleteAppt);
    End;
end;

procedure TJvTFScheduleManager.dbRefreshAppt(Appt: TJvTFAppt);
begin
  If Assigned(Appt) Then
    Begin
      FRefreshing := True;
      Try
        Appt.Notify(Self, sncRefresh);
        If Assigned(FOnRefreshAppt) Then
          FOnRefreshAppt(Self, Appt);
        If RefreshAutoReconcile Then
          ReconcileRefresh(Appt);
      Finally
        FRefreshing := False;

        // BUG - IT'S A LITTLE LATE TO BE USING THE APPT AS A REFRESH TRIGGER!!!
        //RefreshConnections(Appt);
        // Use nil as trigger to refresh everything
        RefreshConnections(nil);
      End;
    End;
{
  If Assigned(Appt) Then
    RefreshAppt(Appt);
}
end;

function TJvTFScheduleManager.dbNewAppt(ID: String): TJvTFAppt;
Var
  New : Boolean;
begin
  Result := nil;
  RequestAppt(ID, Result, New);
  If not New Then
    Raise EJvTFScheduleManagerError.Create('Could not create new appointment. ' +
                                      'Appointment with given ID already exists');
end;

procedure TJvTFScheduleManager.PostAppts;
Var
  I : Integer;
begin
  For I := 0 to ApptCount - 1 do
    dbPostAppt(Appts[I]);
end;

procedure TJvTFScheduleManager.RefreshAppts;
Var
  I : Integer;
  ApptIDList : TStringList;
  Appt : TJvTFAppt;
begin
  // In a multi-user environment, appt objects may be deleted as a result
  // of calling dbRefreshAppt.  (Component user may call Appt.Free.)
  // To account for this we need to build a list of appt ID's instead of
  // working directly from the ScheduleManager's appointment list.

  ApptIDList := TStringList.Create;
  Try
    For I := 0 to ApptCount - 1 do
      Begin
        Appt := Appts[I];
        ApptIDList.Add(Appt.ID);
      End;

    For I := 0 to ApptIDList.Count - 1 do
      Begin
        Appt := FindAppt(ApptIDList[I]);
        If Assigned(Appt) Then
          dbRefreshAppt(Appt);
      End;

    RefreshConnections(nil);
  Finally
    ApptIDList.Free;
  End;
end;


procedure TJvTFScheduleManager.RefreshConnections(Trigger: TObject);
var
  Sched : TJvTFSched;
  Appt : TJvTFAppt;
  I : Integer;
begin
  // Do not refresh if we're loading or refreshing appts
  If FLoadingAppts or Refreshing Then
    Exit;

  If Trigger = nil Then
    Begin
      // refresh all schedules for all controls connected to ScheduleManager
      For I := 0 to ConControlCount - 1 do
        RequestRefresh(ConControls[I], nil);
      // refresh all schedules for all components connected to the ScheduleManager
      For I := 0 to ConComponentCount - 1 do
        {$IFDEF COMPILER3}
        ComponentRequestRefresh(ConComponents[I], nil);
        {$ELSE}
        RequestRefresh(ConComponents[I], nil);
        {$ENDIF}
    End
  Else If Trigger is TJvTFComponent Then
    Begin
      // refresh all schedules for given component
      {$IFDEF COMPILER3}
      ComponentRequestRefresh(TJvTFComponent(Trigger), nil);
      {$ELSE}
      RequestRefresh(TJvTFComponent(Trigger), nil);
      {$ENDIF}
    End
  Else If Trigger is TJvTFControl Then
    Begin
      // refresh all schedules for given control
      RequestRefresh(TJvTFControl(Trigger), nil);
    End
  Else If Trigger is TJvTFSched Then
    Begin
      // refresh all appt controls connected to schedule
      Sched := TJvTFSched(Trigger);
      For I := 0 to Sched.ConControlCount - 1 do
        RequestRefresh(Sched.ConControls[I], Sched);
      // refresh all utf components connected to schedule
      For I := 0 to Sched.ConComponentCount - 1 do
        {$IFDEF COMPILER3}
        ComponentRequestRefresh(Sched.ConComponents[I], Sched);
        {$ELSE}
        RequestRefresh(Sched.ConComponents[I], Sched);
        {$ENDIF}
    End
  Else If Trigger is TJvTFAppt Then
    Begin
      // refresh all appt controls for all schedules connected to this appt
      Appt := TJvTFAppt(Trigger);
      For I := 0 to Appt.ConnectionCount - 1 do
        RefreshConnections(Appt.Connections[I]);
    End
  Else
    Raise EJvTFScheduleManagerError.Create('Invalid Trigger for RefreshControls')
end;

procedure TJvTFScheduleManager.Flush(All: Boolean); //param All defaults to False
var
  I : Integer;
  Sched : TJvTFSched;
  MRUList : TStringList;
  CacheTimeUp : Boolean;
begin
  If FFlushing or FDestroying Then
    Exit;

  FFlushing := True;
  Try
    If All Then
      Begin
        I := 0;
        While I < ScheduleCount do
          Begin
            Sched := Schedules[I];
            If Sched.Cached and not Sched.Persistent Then
              Begin
                If not FlushObject(Sched) Then
                  Inc(I);
              End
            Else
              Inc(I);
          End;
        FlushAppts;
      End
    Else If Cache.CacheType = ctTimed Then
      Begin
        I := 0;
        While I < ScheduleCount do
          Begin
            Sched := Schedules[I];
            CacheTimeUp := Windows.GetTickCount - Sched.CachedTime >=
                           UINT(Cache.TimedDelay);
            If Sched.Cached and CacheTimeUp Then
              Begin
                If not FlushObject(Sched) Then
                  Inc(I);
              End
            Else
              Inc(I);
          End;
        FlushAppts;
      End
    Else If Cache.CacheType = ctBuffer Then
      Begin
        MRUList := TStringList.Create;
        Try
          MRUList.Sorted := True;
          MRUList.Duplicates := dupAccept;
          For I := 0 to ScheduleCount - 1 do
            Begin
              Sched := Schedules[I];
              If Sched.Cached Then
                MRUList.AddObject(IntToHex(Sched.CachedTime, 8), Sched);
            End;
          For I := 0 to MRUList.Count - 1 - Cache.BufferCount do
            FlushObject(MRUList.Objects[I]);
          FlushAppts;
        Finally
          MRUList.Free;
        End;
      End;

  Finally
    FFlushing := False;
  End;
end;

procedure TJvTFScheduleManager.dbRefreshAll;
var
  I : Integer;
begin
  FRefreshing := True;
  Try
    For I := 0 to ApptCount - 1 do
      NotifyAppt(Appts[I], Self, sncRefresh);
    If Assigned(FOnRefreshAll) Then
      FOnRefreshAll(Self);
    If RefreshAutoReconcile Then
      ReconcileRefresh(Self);
  Finally
    FRefreshing := False;
    RefreshConnections(nil);
  End;
end;

procedure TJvTFScheduleManager.dbRefreshOrphans;
var
  I : Integer;
begin
  For I := 0 to ApptCount - 1 do
    If Appts[I].ConnectionCount = 0 Then
      dbRefreshAppt(Appts[I]);
end;

procedure TJvTFScheduleManager.dbRefreshSched(Sched: TJvTFSched);
var
  I : Integer;
begin
  If Assigned(Sched) Then
    Begin
      FRefreshing := True;
      Try
        For I := 0 to Sched.ApptCount - 1 do
          NotifyAppt(Sched.Appts[I], Self, sncRefresh);
        If Assigned(FOnRefreshSched) Then
          FOnRefreshSched(Self, Sched);
        If RefreshAutoReconcile Then
          ReconcileRefresh(Sched);
      Finally
        FRefreshing := False;
        RefreshConnections(Sched);
      End;
    End;
end;

procedure TJvTFScheduleManager.SeTJvTFSchedLoadMode(Value: TJvTFSchedLoadMode);
begin
  If (Value <> FSchedLoadMode) and (Value = slmOnDemand) Then
    // make sure we process any queued batches before changing mode
    ProcessBatches;

  FSchedLoadMode := Value;
end;

procedure TJvTFScheduleManager.AddToBatch(aSched: TJvTFSched);
var
  SchedID : String;
begin
  SchedID := TJvTFScheduleManager.GetScheduleID(aSched.SchedName, aSched.SchedDate);
  FSchedBatch.AddObject(SchedID, aSched);
end;

procedure TJvTFScheduleManager.ProcessBatches;
var
  I : Integer;
  aSched : TJvTFSched;
  CompName : String;
  CompDate : TDate;
  BatchName : String;
  BatchStartDate : TDate;
  BatchEndDate : TDate;

                 /////////////////////////////////////////////
                 //        SUBORDINATE ROUTINES
                 /////////////////////////////////////////////
                 procedure UpdateCompares(_aSched : TJvTFSched);
                 begin
                   CompName := _aSched.SchedName;
                   CompDate := _aSched.SchedDate;
                 end;

                 procedure NewBatch(_aSched : TJvTFSched);
                 begin
                   BatchName := _aSched.SchedName;
                   BatchStartDate := _aSched.SchedDate;
                   BatchEndDate := _aSched.SchedDate;
                 end;

////////////////
// MAIN ROUTINE
////////////////
begin
  If FSchedBatch.Count = 0 Then
    Exit;

  // added by Mike 1/14/01
  FLoadingAppts := True;
  try
    // Prime the process (reminds me of COBOL - yuck!)
    aSched := TJvTFSched(FSchedBatch.Objects[0]);
    UpdateCompares(aSched);
    NewBatch(aSched);

    For I := 1 to FSchedBatch.Count - 1 do
      Begin
        aSched := TJvTFSched(FSchedBatch.Objects[I]);

        If (aSched.SchedName <> CompName) or
           (Trunc(aSched.SchedDate) - 1 <> Trunc(CompDate)) Then
          Begin
            // Hit new batch.  Load the current batch and then
            // set batch info to new batch.
            LoadBatch(BatchName, BatchStartDate, BatchEndDate);
            NewBatch(aSched);
          End
        Else
          // Still in current batch.  Update the batch end date.
          BatchEndDate := aSched.SchedDate;

        UpdateCompares(aSched);
      End;

    // Load the last batch
    LoadBatch(BatchName, BatchStartDate, BatchEndDate);

    FSchedBatch.Clear;

    // ADD OnBatchesProcessed EVENT HERE !!
    If Assigned(FOnBatchesProcessed) Then
      FOnBatchesProcessed(Self);
  finally
    // added by Mike 1/14/01
    FLoadingAppts := False;
    // added by Mike 1/14/01
    RefreshConnections(nil);
  end;
end;

procedure TJvTFScheduleManager.LoadBatch(BatchName: String; BatchStartDate,
  BatchEndDate: TDate);
begin
  If Assigned(FOnLoadBatch) Then
    FOnLoadBatch(Self, BatchName, BatchStartDate, BatchEndDate);
end;

function TJvTFScheduleManager.QueryPostAppt(Appt: TJvTFAppt): Boolean;
begin
  Result := True;
  If Assigned(FOnPostApptQuery) Then
    FOnPostApptQuery(Self, Appt, Result);
end;

function TJvTFScheduleManager.GetApptDisplayText(aComponent: TComponent;
  Appt: TJvTFAppt): String;
begin
  If Assigned(Appt) Then
    Result := Appt.Description
  Else
    Result := '';

  If Assigned(FOnGetApptDisplayText) Then
    FOnGetApptDisplayText(Self, aComponent, Appt, Result);
end;

procedure TJvTFScheduleManager.SetApptDescription(Appt: TJvTFAppt;
  var Value: String);
begin
  If Assigned(FOnSetApptDescription) Then
    FOnSetApptDescription(Self, Appt, Value);
end;

procedure TJvTFScheduleManager.GetApptDescription(Appt: TJvTFAppt;
  var Value: String);
begin
  If Assigned(FOnGetApptDescription) Then
    FOnGetApptDescription(Self, Appt, Value);
end;

function TJvTFScheduleManager.GetApptClass: TJvTFApptClass;
begin
  Result := TJvTFAppt;
end;

function TJvTFScheduleManager.GetSchedClass: TJvTFSchedClass;
begin
  Result := TJvTFSched;
end;

procedure TJvTFScheduleManager.ReconcileRefresh(Scope: TObject);
var
  Appt : TJvTFAppt;
  Sched : TJvTFSched;
  I : Integer;
begin
  If Scope is TJvTFAppt Then
    Begin
      Appt := TJvTFAppt(Scope);
      If not Appt.Refreshed Then
        Appt.ClearSchedules;
    End
  Else If Scope is TJvTFSched Then
    Begin
      Sched := TJvTFSched(Scope);
      I := 0;
      While I < Sched.ApptCount do
        Begin
          Appt := Sched.Appts[I];
          If not Appt.Refreshed Then
            Appt.ClearSchedules
          Else
            Inc(I);
        End;
    End
  Else If Scope is TJvTFScheduleManager Then
    For I := 0 to ApptCount - 1 do
      ReconcileRefresh(Appts[I])
  Else
    Raise EJvTFScheduleManagerError.Create('Invalid Scope in ReconcileRefresh');
end;

procedure TJvTFScheduleManager.SetRefreshAutoReconcile(Value: Boolean);
begin
  FRefreshAutoReconcile := Value;
end;

{ TJvTFHint }
constructor TJvTFHint.Create(anApptCtrl: TJvTFControl);
begin
  Inherited Create(anApptCtrl);
  FApptCtrl := anApptCtrl;
  FTimer := TTimer.Create(Self);
  FShortPause := 1000;
  FPause := 3000;
  FTimer.OnTimer := TimerOnTimer;
  PrepTimer(True);
end;

destructor TJvTFHint.Destroy;
begin
  FTimer.Free;
  Inherited;
end;

procedure TJvTFHint.SetPause(Value: Integer);
begin
  FPause := Value;
end;

procedure TJvTFHint.SetShortPause(Value: Integer);
begin
  FShortPause := Value;
end;

procedure TJvTFHint.TimerOnTimer(Sender: TObject);
begin
  FTimer.Enabled := False;

  If FShortTimer Then
    DoHint(False)
  Else
    Begin
      ReleaseHandle;
      PrepTimer(True);
    End;
end;

procedure TJvTFHint.PrepTimer(Short: Boolean);
begin
  ReleaseHandle;
  FShortTimer := Short;
  If Short Then
    FTimer.Interval := FShortPause
  Else
    FTimer.Interval := FPause;
end;

procedure TJvTFHint.SetHintText(StartDate, EndDate: TDate; StartTime,
  EndTime: TTime; Desc: String; ShowDatesTimes, ShowDesc: Boolean);
var
  ShowDates : Boolean;
  HintText,
  DFormat,
  TFormat : String;
begin
  HintText := '';
  If ShowDatesTimes Then
    Begin
      DFormat := FApptCtrl.DateFormat;
      TFormat := FApptCtrl.TimeFormat;
      ShowDates := Trunc(StartDate) <> Trunc(EndDate);

      If ShowDates Then
        HintText := FormatDateTime(DFormat, StartDate) + ' ';
      HintText := HintText + FormatDateTime(TFormat, StartTime) + ' - ';
      If ShowDates Then
        HintText := HintText + FormatDateTime(DFormat, EndDate) + ' ';
      HintText := HintText + FormatDateTime(TFormat, EndTime);
    End;

  If ShowDesc Then
    Begin
      If HintText <> '' Then
        HintText := HintText + #13#10;
      HintText := HintText + Desc;
    End;
  FHintText := HintText;
end;

procedure TJvTFHint.DoHint(Sustained: Boolean);
var
  Ref : TObject;
begin
  PropertyCheck;
  {
  If Assigned(FOnShowHint) Then
    FOnShowHint(Self, HintType, FHintRect, FHintText);
  }

  If Assigned(FOnShowHint) Then
    Begin
      If HintType = shtAppt Then
        Ref := FOldAppt
      Else If HintType = shtObj Then
        Ref := FOldObj
      Else
        Ref := nil;

      FOnShowHint(Self, HintType, Ref, FHintRect, FHintText);
    End;

  If not Windows.IsRectEmpty(FHintRect) and (FHintText <> '') Then
    If Sustained Then
      Begin
        Inherited ActivateHint(FHintRect, FHintText);
      End
    Else
      ActivateHint(FHintRect, FHintText);
end;

procedure TJvTFHint.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := WindowClass.Style and not CS_SAVEBITS;
  end;
end;

procedure TJvTFHint.ActivateHint(Rect: TRect; const AHint: String);
begin
  PrepTimer(False);
  Inherited;
  // Reset the timer so we get the full interval
  FTimer.Enabled := False;
  FTimer.Enabled := True;
end;

procedure TJvTFHint.ApptHint(Appt: TJvTFAppt; X, Y: Integer; ShowDatesTimes,
  ShowDesc, FormattedDesc: Boolean);
var
  HintTopLeft : TPoint;
  Immediate : Boolean;
  ApptDesc : String;
begin
  If Appt <> FOldAppt Then
    Begin
      FHintType := shtAppt;
      Immediate := not FShortTimer;
      FHintCell := Point(-100, -100);
      FOldAppt := Appt;
      If Assigned(Appt) Then
        Begin
          ApptDesc := Appt.Description;
          If not FormattedDesc Then
            ApptDesc := StripCRLF(ApptDesc);
          SetHintText(Appt.StartDate, Appt.EndDate, Appt.StartTime, Appt.EndTime,
                      ApptDesc, ShowDatesTimes, ShowDesc);
          FHintRect := CalcHintRect(FApptCtrl.Width, FHintText, nil);
          HintTopLeft := FApptCtrl.ClientToScreen(Point(X, Y));
          FHintRect := MoveRect(FHintRect, HintTopLeft.X, HintTopLeft.Y);
          If Immediate Then
            DoHint(False)
          Else
            Begin
              PrepTimer(True);
              FTimer.Enabled := True;
            End;
        End
      Else
        Begin
          ReleaseHandle;
          PrepTimer(True);
        End;
    End;
end;

procedure TJvTFHint.StartEndHint(StartDate, EndDate: TDate; StartTime,
  EndTime: TTime; X, Y: Integer; ShowDates: Boolean);
var
  HintTopLeft : TPoint;
begin
  FHintType := shtStartEnd;
  SetHintText(StartDate, EndDate, StartTime, EndTime, '', True, False);
  FHintRect := CalcHintRect(FApptCtrl.Width, FHintText, nil);
  HintTopLeft := FApptCtrl.ClientToScreen(Point(X, Y));
  FHintRect := MoveRect(FHintRect, HintTopLeft.X, HintTopLeft.Y);
  If HandleAllocated and Showing Then
    BoundsRect := FHintRect
  Else
    DoHint(True);
end;

procedure TJvTFHint.CellHint(Row, Col: Integer; HintText: String; CellRect: TRect);
var
  Immediate : Boolean;
  DiffCell : Boolean;
begin
  DiffCell := (Row <> FHintCell.Y) or (Col <> FHintCell.X);
  If DiffCell or not FTimer.Enabled Then
    Begin
      FHintType := shtCell;
      FOldAppt := nil;
      ReleaseHandle;
      FHintCell.X := Col;
      FHintCell.Y := Row;
      Immediate := not FShortTimer;
      FHintText := HintText;
      //If (FHintText <> '') and DiffCell Then
      If FHintText <> '' Then
        Begin
          CellRect.TopLeft := FApptCtrl.ClientToScreen(CellRect.TopLeft);
          CellRect.BottomRight := FApptCtrl.ClientToScreen(CellRect.BottomRight);
          FHintRect := CalcHintRect(FApptCtrl.Width, FHintText, nil);
          FHintRect := CenterRect(CellRect, FHintRect);
          If Immediate Then
            DoHint(False)
          Else
            Begin
              PrepTimer(True);
              FTimer.Enabled := True;
            End;
        End
      Else
        Begin
          ReleaseHandle;
          PrepTimer(True);
        End;
    End;
end;

procedure TJvTFHint.ReleaseHandle;
begin
  FTimer.Enabled := False;
  DestroyHandle;
end;


procedure TJvTFHint.PropertyCheck;
begin
  If Assigned(RefProps) Then
    Begin
      If RefProps.HintColor = clDefault Then
        Color := Application.HintColor
      Else
        Color := RefProps.HintColor;

      If RefProps.HintHidePause = -1 Then
        Pause := Application.HintHidePause
      Else
        Pause := RefProps.HintHidePause;

      If RefProps.HintPause = -1 Then
        ShortPause := Application.HintPause
      Else
        ShortPause := RefProps.HintPause;
    End;
end;

procedure TJvTFHint.MultiLineObjHint(Obj: TObject; X, Y: Integer;
  Hints: TStrings);
var
  Immediate : Boolean;
  HintTopLeft : TPoint;
begin
  If Obj <> FOldObj Then
    Begin
      FOldAppt := nil;
      FHintType := shtObj;
      Immediate := not FShortTimer;
      FHintCell := Point(-100, -100);
      FOldObj := Obj;
      If Assigned(Obj) and (Hints.Count > 0) Then
        Begin
          FHintText := Hints.Text;
          FHintRect := CalcHintRect(FApptCtrl.Width, FHintText, nil);
          HintTopLeft := FApptCtrl.ClientToScreen(Point(X + 8, Y + 16));
          FHintRect := MoveRect(FHintRect, HintTopLeft.X, HintTopLeft.Y);

          If Immediate Then
            DoHint(False)
          Else
            Begin
              PrepTimer(True);
              FTimer.Enabled := True;
            End;
        End
      Else
        Begin
          ReleaseHandle;
          PrepTimer(True);
        End;
    End;
end;

{ TJvTFControl }
constructor TJvTFControl.Create(AOwner: TComponent);
begin
  Inherited;

  FSchedules := TStringList.Create;
  FTimeFormat := 't'; // global short time format
  FDateFormat := 'ddddd'; // global short date format
end;

destructor TJvTFControl.Destroy;
begin
  ScheduleManager := nil;
  FSchedules.Free;

  Inherited;
end;

procedure TJvTFControl.SetManager(Value: TJvTFScheduleManager);
begin
  If Value <> FScheduleManager Then
    Begin
      If Assigned(FScheduleManager) Then
        FScheduleManager.Notify(Self, sncDisconnectControl);
      FScheduleManager := nil;

      If Assigned(Value) Then
        Value.Notify(Self, sncConnectControl);
      FScheduleManager := Value;
    End;
end;

function TJvTFControl.GetSchedule(Index: Integer): TJvTFSched;
begin
  Result := TJvTFSched(FSchedules.Objects[Index]);
end;

procedure TJvTFControl.SetDateFormat(Value: String);
begin
  If FDateFormat <> Value Then
    Begin
      FDateFormat := Value;
      Invalidate;
    End;
end;

procedure TJvTFControl.SetTimeFormat(Value: String);
begin
  If FTimeFormat <> Value Then
    Begin
      FTimeFormat := Value;
      Invalidate;
    End;
end;

procedure TJvTFControl.Notify(Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  Case Code of
    sncRequestSchedule : ReqSchedNotification(TJvTFSched(Sender));
    sncReleaseSchedule : RelSchedNotification(TJvTFSched(Sender));
    sncRefresh         : RefreshControl;
    sncDestroyAppt     : DestroyApptNotification(TJvTFAppt(Sender));
    sncDestroySchedule : DestroySchedNotification(TJvTFSched(Sender));
  End;
end;

procedure TJvTFControl.ReqSchedNotification(Schedule: TJvTFSched);
Var
  SchedID : String;
begin
  SchedID := TJvTFScheduleManager.GetScheduleID(Schedule.SchedName, Schedule.SchedDate);
  If FSchedules.IndexOf(SchedID) = -1 Then
    FSchedules.AddObject(SchedID, Schedule);
end;

procedure TJvTFControl.RelSchedNotification(Schedule: TJvTFSched);
Var
  I : Integer;
begin
  I := FSchedules.IndexOfObject(Schedule);
  If I > -1 Then
    FSchedules.Delete(I);
end;

procedure TJvTFControl.NotifyManager(Serv: TJvTFScheduleManager;
  Sender: TObject; Code: TJvTFServNotifyCode);
begin
  If Assigned(Serv) Then
    Serv.Notify(Sender, Code)
  Else
    Raise EJvTFScheduleManagerError.Create('ScheduleManager notification failed.  ScheduleManager not assigned');
end;

procedure TJvTFControl.CNRequestRefresh(var Msg : TCNRequestRefresh);
begin
  Invalidate;
end;

procedure TJvTFControl.RefreshControl;
begin
  Invalidate;
end;

procedure TJvTFControl.DestroyApptNotification(anAppt: TJvTFAppt);
begin
  // do nothing, leave implementation to successors
end;

procedure TJvTFControl.DestroySchedNotification(aSched: TJvTFSched);
begin
  // do nothing, leave implementation to successors
end;

procedure TJvTFControl.DoStartDrag(var DragObject: TDragObject);
begin
  Inherited;

  FDragInfo := TJvTFDragInfo.Create;
  With FDragInfo do
    Begin
      ApptCtrl := Self;
      Shift := Self.FShift;
    End;

{
Originally, a specific drag object was created and given to the DragObject
param.  This worked fine.  Because of differences in the VCL DragObject
hierarachy between D3 and D4, the decision was made to move away from
using a drag object.

  FDragAppt := TDragAppt.Create(Self);
  With FDragAppt do
    Begin
      ApptCtrl := Self;
      Schedule := SelSchedule;
      Appt := SelAppt;
      Shift := FDragShift;
    End;
  DragObject := FDragAppt;
}
end;

procedure TJvTFControl.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  Inherited;

  FDragInfo.Free;
  FDragInfo := nil;
end;

procedure TJvTFControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  Inherited;
  FShift := Shift;
end;

function TJvTFControl.ScheduleCount: Integer;
begin
  Result := FSchedules.Count;
end;

function TJvTFControl.FindSchedule(SchedName: String;
  SchedDate: TDate): TJvTFSched;
Var
  I : Integer;
begin
  Result := nil;

  I := FSchedules.IndexOf(TJvTFScheduleManager.GetScheduleID(SchedName, SchedDate));
  If I > -1 Then
    Result := TJvTFSched(FSchedules.Objects[I]);
end;

function TJvTFControl.RetrieveSchedule(SchedName: String;
  SchedDate: TDate): TJvTFSched;
begin
  Result := FindSchedule(SchedName, SchedDate);

  If not Assigned(Result) Then
    If Assigned(ScheduleManager) Then
      Result := ScheduleManager.RequestSchedule(Self, SchedName, SchedDate)
    Else
      Raise EJvTFScheduleManagerError.Create('Could not retrieve schedule.  ' +
                                        'ScheduleManager not assigned');
end;

procedure TJvTFControl.ReleaseSchedule(SchedName: String;
  SchedDate: TDate);
Var
  SchedID : String;
begin
  SchedID := TJvTFScheduleManager.GetScheduleID(SchedName, SchedDate);
  If FSchedules.IndexOf(SchedID) > -1 Then
    If Assigned(ScheduleManager) Then
      ScheduleManager.ReleaseSchedule(Self, SchedName, SchedDate)
    Else
      Raise EJvTFScheduleManagerError.Create('Could not release schedule.  ' +
                                        'ScheduleManager not assigned');

end;

procedure TJvTFControl.ReleaseSchedules;
begin
  While ScheduleCount > 0 do
    ReleaseSchedule(Schedules[0].SchedName, Schedules[0].SchedDate);
end;

procedure TJvTFControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
//  If (AComponent = Navigator) and (Operation = opRemove) Then
//    Navigator := nil;
end;

//procedure TJvTFControl.SetNavigator(Value: TJvTFNavigator);
//begin
//  If Value <> FNavigator Then
//    Begin
//      If Assigned(FNavigator) Then
//        FNavigator.UnregisterControl(Self);
//      FNavigator := nil;
//
//      If Assigned(Value) Then
//        Value.RegisterControl(Self);
//      FNavigator := Value;
//    End;
//end;

procedure TJvTFControl.Navigate(aControl: TJvTFControl;
  SchedNames: TStringList; Dates: TJvTFDateList);
begin
//  If Assigned(FOnNavigate) Then
//    FOnNavigate(Self, aControl, SchedNames, Dates);
end;

procedure TJvTFControl.ProcessBatches;
begin
  If Assigned(ScheduleManager) and (ScheduleManager.SchedLoadMode = slmBatch) Then
    ScheduleManager.ProcessBatches;
end;

{ TJvTFComponent }

constructor TJvTFComponent.Create(AOwner: TComponent);
begin
  inherited;

  FSchedules := TStringList.Create;
  FTimeFormat := 't'; // global short time format
  FDateFormat := 'ddddd'; // global short date format
end;

destructor TJvTFComponent.Destroy;
begin
  ScheduleManager := nil;
  FSchedules.Free;

  inherited;
end;

procedure TJvTFComponent.DestroyApptNotification(anAppt: TJvTFAppt);
begin
  // do nothing, leave implementation to descendants
end;

procedure TJvTFComponent.DestroySchedNotification(aSched: TJvTFSched);
begin
  // do nothing, leave implementation to descendants
end;

function TJvTFComponent.FindSchedule(SchedName: String;
  SchedDate: TDate): TJvTFSched;
var
  I : Integer;
begin
  Result := nil;

  I := FSchedules.IndexOf(TJvTFScheduleManager.GetScheduleID(SchedName, SchedDate));
  If I > -1 Then
    Result := TJvTFSched(FSchedules.Objects[I]);
end;

function TJvTFComponent.GetSchedule(Index: Integer): TJvTFSched;
begin
  Result := TJvTFSched(FSchedules.Objects[Index]);
end;

procedure TJvTFComponent.Notify(Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  Case Code of
    sncRequestSchedule : ReqSchedNotification(TJvTFSched(Sender));
    sncReleaseSchedule : RelSchedNotification(TJvTFSched(Sender));
    sncRefresh         : RefreshComponent;
    sncDestroyAppt     : DestroyApptNotification(TJvTFAppt(Sender));
    sncDestroySchedule : DestroySchedNotification(TJvTFSched(Sender));
  End;
end;

procedure TJvTFComponent.NotifyManager(Serv: TJvTFScheduleManager; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  If Assigned(Serv) Then
    Serv.Notify(Sender, Code)
  Else
    Raise EJvTFScheduleManagerError.Create('ScheduleManager notification failed.  ScheduleManager not assigned');
end;

procedure TJvTFComponent.ProcessBatches;
begin
  If Assigned(ScheduleManager) and (ScheduleManager.SchedLoadMode = slmBatch) Then
    ScheduleManager.ProcessBatches;
end;

procedure TJvTFComponent.RefreshComponent;
begin
  // do nothing, leave implementation to descendants
end;

procedure TJvTFComponent.ReleaseSchedule(SchedName: String;
  SchedDate: TDate);
var
  SchedID : String;
begin
  SchedID := TJvTFScheduleManager.GetScheduleID(SchedName, SchedDate);
  If FSchedules.IndexOf(SchedID) > -1 Then
    If Assigned(ScheduleManager) Then
      {$IFDEF COMPILER3}
      ScheduleManager.ComponentReleaseSchedule(Self, SchedName, SchedDate)
      {$ELSE}
      ScheduleManager.ReleaseSchedule(Self, SchedName, SchedDate)
      {$ENDIF}
    Else
      Raise EJvTFScheduleManagerError.Create('Could not release schedule.  ' +
        'ScheduleManager not assigned');
end;

procedure TJvTFComponent.ReleaseSchedules;
begin
  While ScheduleCount > 0 do
    ReleaseSchedule(Schedules[0].SchedName, Schedules[0].SchedDate);
end;

procedure TJvTFComponent.RelSchedNotification(Schedule: TJvTFSched);
var
  I : Integer;
begin
  I := FSchedules.IndexOfObject(Schedule);
  If I > -1 Then
    FSchedules.Delete(I);
end;

procedure TJvTFComponent.ReqSchedNotification(Schedule: TJvTFSched);
var
  SchedID : String;
begin
  SchedID := TJvTFScheduleManager.GetScheduleID(Schedule.SchedName, Schedule.SchedDate);
  If FSchedules.IndexOf(SchedID) = -1 Then
    FSchedules.AddObject(SchedID, Schedule);
end;

function TJvTFComponent.RetrieveSchedule(SchedName: String;
  SchedDate: TDate): TJvTFSched;
begin
  Result := FindSchedule(SchedName, SchedDate);

  If not Assigned(Result) Then
    If Assigned(ScheduleManager) Then
      {$IFDEF COMPILER3}
      Result := ScheduleManager.ComponentRequestSchedule(Self, SchedName, SchedDate)
      {$ELSE}
      Result := ScheduleManager.RequestSchedule(Self, SchedName, SchedDate)
      {$ENDIF}
    Else
      Raise EJvTFScheduleManagerError.Create('Could not retrieve schedule.  ' +
        'ScheduleManager not assigned');
end;

function TJvTFComponent.ScheduleCount: Integer;
begin
  Result := FSchedules.Count;
end;

procedure TJvTFComponent.SetDateFormat(Value: String);
begin
  FDateFormat := Value;
end;

procedure TJvTFComponent.SetManager(Value: TJvTFScheduleManager);
begin
  If Value <> FScheduleManager Then
    Begin
      If Assigned(FScheduleManager) Then
        FScheduleManager.Notify(Self, sncDisconnectComponent);
      FScheduleManager := nil;

      If Assigned(Value) Then
        Value.Notify(Self, sncConnectComponent);
      FScheduleManager := Value;
    End;
end;

procedure TJvTFComponent.SetTimeFormat(Value: String);
begin
  FTimeFormat := Value;
end;

procedure TJvTFComponent.UpdateDesigner;
var
  ParentForm: TCustomForm;
begin
  if (csDesigning in ComponentState) and not (csUpdating in ComponentState) then
    begin
      Try
        ParentForm := TCustomForm(Owner);
        if Assigned(ParentForm) and Assigned(ParentForm.Designer) then
          ParentForm.Designer.Modified;
      Except
        // handle the exception by doing nothing
      End;
    end;
end;

{ TJvTFPrinter }

procedure TJvTFPrinter.AbortPrint;
begin
  If Printer.Printing Then
    Printer.Abort
  Else
    FAborted := True;
end;

function TJvTFPrinter.ConvertMeasure(Value: Integer; FromMeasure,
  ToMeasure: TJvTFPrinterMeasure; Horizontal: Boolean): Integer;
const
  MMFactor = 2.54;
var
  PPI : Integer;
begin
  If Horizontal Then
    PPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSX)
  Else
    PPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSY);

  If (FromMeasure = pmPixels) and (ToMeasure = pmInches) Then
    Result := Round(Value / PPI * 100)
  Else If (FromMeasure = pmPixels) and (ToMeasure = pmMM) Then
    Result := Round(Value / PPI * 100 * MMFactor)
  Else If (FromMeasure = pmInches) and (ToMeasure = pmPixels) Then
    Result := Round(Value / 100 * PPI)
  Else If (FromMeasure = pmInches) and (ToMeasure = pmMM) Then
    Result := Round(Value * MMFactor)
  Else If (FromMeasure = pmMM) and (ToMeasure = pmPixels) Then
    Result := Round(Value / MMFactor / 100 * PPI)
  Else If (FromMeasure = pmMM) and (ToMeasure = pmInches) Then
    Result := Round(Value / MMFactor)
  Else
    Result := Value;
end;

constructor TJvTFPrinter.Create(AOwner: TComponent);
begin
  inherited;

  CreateLayout;
  FMeasure := pmInches;
  FPages := TStringList.Create;
  FBodies := TStringList.Create;
  InitializeMargins;
end;

procedure TJvTFPrinter.CreateDoc;
begin
  If State = spsNoDoc Then
    Begin
      FState := spsCreating;
      FAborted := False;

      FDocDateTime := Now;
      If DirectPrint Then
        Printer.BeginDoc;
    End
  Else
    Raise EJvTFPrinterError.Create('Could not create a document because a ' +
      'document already exists');
end;

procedure TJvTFPrinter.CreateLayout;
begin
  FPageLayout := TJvTFPrinterPageLayout.Create(Self);
end;

destructor TJvTFPrinter.Destroy;
begin
  FreeDoc;
  FBodies.Free;
  FPages.Free;

  FPageLayout.Free;
  inherited;
end;

procedure TJvTFPrinter.DrawBody(aCanvas : TCanvas; aRect : TRect;
  PageNum : Integer);
begin
  If Assigned(FOnDrawBody) Then
    FOnDrawBody(Self, aCanvas, aRect, PageNum);
end;

procedure TJvTFPrinter.DrawFooter(aCanvas : TCanvas; aRect : TRect;
  PageNum : Integer);
begin
  If Assigned(FOnDrawFooter) Then
    FOnDrawFooter(Self, aCanvas, aRect, PageNum);
end;

procedure TJvTFPrinter.DrawHeader(aCanvas : TCanvas; aRect : TRect;
  PageNum : Integer);
begin
  If Assigned(FOnDrawHeader) Then
    FOnDrawHeader(Self, aCanvas, aRect, PageNum);
end;

procedure TJvTFPrinter.FinishDoc;
var
  I : Integer;
  aCanvas : TMetafileCanvas;
  HeaderRect,
  FooterRect : TRect;
begin
  If Aborted Then
    Exit;

  If State <> spsCreating Then
    Raise EJvTFPrinterError.Create('Could not finish document because no ' +
      'document has been created');

  FPageCount := FBodies.Count;
  FState := spsAssembling;
  Try
    If Assigned(FOnAssembleProgress) Then
      FOnAssembleProgress(Self, 0, FBodies.Count);

    If DirectPrint Then
      Printer.EndDoc
    Else
      Begin
        GetHeaderFooterRects(HeaderRect, FooterRect);
        I := 0;
        While (I < FBodies.Count) and not Aborted do
          Begin
            aCanvas := TMetafileCanvas(FBodies.Objects[I]);

            Try
              DrawHeader(aCanvas, HeaderRect, I + 1);
              DrawFooter(aCanvas, FooterRect, I + 1);
            Finally
              aCanvas.Free;
              FBodies.Objects[I] := nil;
            End;

            If Assigned(FOnAssembleProgress) Then
              FOnAssembleProgress(Self, I + 1, FBodies.Count);

            Inc(I);
            Application.ProcessMessages;
          End;
      End;

    FBodies.Clear;
  Finally
    FState := spsFinished;
  End;
end;

procedure TJvTFPrinter.FreeDoc;
begin
  While FBodies.Count > 0 do
    Begin
      FBodies.Objects[0].Free;
      FBodies.Delete(0);
    End;

  While FPages.Count > 0 do
    Begin
      FPages.Objects[0].Free;
      FPages.Delete(0);
    End;

  FState := spsNoDoc;
end;

function TJvTFPrinter.GetBodyHeight: Integer;  // always in pixels
var
  PhysHeight,
  TopMarginPels,
  BottomMarginPels,
  HeaderPels,
  FooterPels : Integer;
begin
  PhysHeight := Windows.GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
  TopMarginPels := ConvertMeasure(PageLayout.MarginTop, Measure, pmPixels,
    False);
  BottomMarginPels := ConvertMeasure(PageLayout.MarginBottom, Measure, pmPixels,
    False);
  HeaderPels := ConvertMeasure(PageLayout.HeaderHeight, Measure, pmPixels,
    False);
  FooterPels := ConvertMeasure(PageLayout.FooterHeight, Measure, pmPixels,
    False);

  Result := PhysHeight - TopMarginPels - BottomMarginPels -
            HeaderPels - FooterPels;
end;

function TJvTFPrinter.GetBodyLeft: Integer;  // always in pixels
begin
  Result := GetMarginOffset(1);
end;

function TJvTFPrinter.GetBodyTop: Integer;  // always in pixels
begin
  Result := GetMarginOffset(2) +
            ConvertMeasure(PageLayout.HeaderHeight, Measure, pmPixels, False) + 1;
end;

function TJvTFPrinter.GetBodyWidth: Integer;  // always in pixels
var
  PhysWidth,
  LeftMarginPels,
  RightMarginPels : Integer;
begin
  PhysWidth := Windows.GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
  LeftMarginPels := ConvertMeasure(PageLayout.MarginLeft, Measure, pmPixels, True);
  RightMarginPels := ConvertMeasure(PageLayout.MarginRight, Measure, pmPixels, True);

  Result := PhysWidth - LeftMarginPels - RightMarginPels;
end;

function TJvTFPrinter.GetDocDateTime: TDateTime;
begin
  If State = spsNoDoc Then
    Raise EJvTFPrinterError.Create('Document does not exist');

  Result := FDocDateTime;
end;

procedure TJvTFPrinter.GetHeaderFooterRects(var HeaderRect,
  FooterRect: TRect);
begin
  HeaderRect.Left := FMarginOffsets.Left;
  HeaderRect.Top := FMarginOffsets.Top;
  HeaderRect.Right := HeaderRect.Left + BodyWidth;
  HeaderRect.Bottom := HeaderRect.Top + ConvertMeasure(PageLayout.HeaderHeight,
    Measure, pmPixels, False);

  FooterRect.Left := HeaderRect.Left;
  FooterRect.Right := HeaderRect.Right;
  FooterRect.Top := BodyTop + BodyHeight;
  FooterRect.Bottom := FooterRect.Top + ConvertMeasure(PageLayout.FooterHeight,
    Measure, pmPixels, False);
end;

function TJvTFPrinter.GetMarginOffset(Index: Integer): Integer;
begin
  Case Index of
    1 : Result := FMarginOffsets.Left;
    2 : Result := FMarginOffsets.Top;
    3 : Result := FMarginOffsets.Right;
  Else
    Result := FMarginOffsets.Bottom;
  End;
end;

function TJvTFPrinter.GetPage(Index: Integer): TMetafile;
begin
  If DirectPrint Then
    Raise EJvTFPrinterError.Create('Document pages cannot be accessed if ' +
      'printing directly to the printer');
      
  If State <> spsFinished Then
    Raise EJvTFPrinterError.Create('Document pages are inaccessible until ' +
      'the document has been finished');
  Result := TMetafile(FPages.Objects[Index]);
end;

function TJvTFPrinter.GetPageCount: Integer;
begin
  Case State of
    spsNoDoc : Raise EJvTFPrinterError.Create('Could not retrieve page count ' +
      'because document does not exist');
    spsCreating : Result := FBodies.Count;
    spsAssembling : Result := FPageCount;
    spsFinished : Result := FPages.Count;
  Else
    Result := -1;
  End;
end;

function TJvTFPrinter.GetUnprintable: TJvTFMargins;
var
  LeftMarg,
  TopMarg,
  WidthPaper,
  HeightPaper,
  WidthPrintable,
  HeightPrintable : Integer;
begin
  LeftMarg := Windows.GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
  TopMarg := Windows.GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);
  WidthPaper := Windows.GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
  HeightPaper := Windows.GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
  WidthPrintable := Printer.PageWidth;
  HeightPrintable := Printer.PageHeight;

  With Result do
    Begin
      Left := LeftMarg;
      Top := TopMarg;
      Right := WidthPaper - WidthPrintable - LeftMarg;
      Bottom := HeightPaper - HeightPrintable - TopMarg;
    End;
end;

procedure TJvTFPrinter.InitializeMargins;
var
  I,
  Unprintable,
  NewMargin : Integer;
  Horz : Boolean;
begin
  For I := 1 to 4 do
    Begin
      SetMarginOffset(I, 0);

      Case I of
        1 : Unprintable := GetUnprintable.Left;
        2 : Unprintable := GetUnprintable.Top;
        3 : Unprintable := GetUnprintable.Right;
      Else
        Unprintable := GetUnprintable.Bottom;
      End;

      Horz := (I = 1) or (I = 3);
      NewMargin := ConvertMeasure(Unprintable, pmPixels, Measure, Horz);

      Case I of
        1 : PageLayout.FMargins.Left := NewMargin;
        2 : PageLayout.FMargins.Top := NewMargin;
        3 : PageLayout.FMargins.Right := NewMargin;
      Else
        PageLayout.FMargins.Bottom := NewMargin;
      End;
    End;
end;

procedure TJvTFPrinter.MarginError;
begin
  If Assigned(FOnMarginError) Then
    FOnMarginError(Self);
end;

procedure TJvTFPrinter.NewDoc;
begin
  FreeDoc;
  CreateDoc;
end;

procedure TJvTFPrinter.NewPage;
var
  aMetafile : TMetafile;
  aCanvas : TCanvas;
  HeaderRect,
  FooterRect : TRect;
begin
  If Aborted Then
    Exit;

  If DirectPrint Then
    Begin
      If PageCount > 0 Then
        Printer.NewPage;
      aCanvas := Printer.Canvas;
      FPages.Add('');
    End
  Else
    Begin
      // Create a TMetafile for the page
      aMetafile := TMetafile.Create;
      FPages.AddObject('', aMetafile);
      // Create a TMetafileCanvas as a canvas for the page.
      // Store the canvas in FBodies so we can retrieve it later to draw
      // the header and footer.
      aCanvas := TMetafileCanvas.Create(aMetafile, Printer.Handle);
    End;

  FBodies.AddObject('', aCanvas);
  aCanvas.Font.PixelsPerInch := Windows.GetDeviceCaps(Printer.Handle,
    LOGPIXELSX);

  Windows.SetViewPortOrgEx(aCanvas.Handle, BodyLeft, Bodytop, nil);
  DrawBody(aCanvas, Rect(BodyLeft, BodyTop, BodyWidth - BodyLeft,
                         BodyHeight - BodyTop), FPages.Count);
  Windows.SetViewPortOrgEx(aCanvas.Handle, 0, 0, nil);

  If DirectPrint Then
    Begin
      GetHeaderFooterRects(HeaderRect, FooterRect);
      DrawHeader(aCanvas, HeaderRect, PageCount);
      DrawFooter(aCanvas, FooterRect, PageCount);
    End;
end;

procedure TJvTFPrinter.Print;
var
  I : Integer;
begin
  If Aborted or DirectPrint Then
    Exit;

  If State <> spsFinished Then
    Raise EJvTFPrinterError.Create('Only a finished document can be printed');
  If PageCount = 0 Then
    Raise EJvTFPrinterError.Create('There are no pages to print');

  If Assigned(FOnPrintProgress) Then
    FOnPrintProgress(Self, 0, PageCount);
  Application.ProcessMessages;

  Printer.Title := Title;
  Printer.BeginDoc;
  If not Printer.Aborted Then
    Printer.Canvas.Draw(0, 0, Pages[0]);

  If Assigned(FOnPrintProgress) Then
    FOnPrintProgress(Self, 1, PageCount);
  Application.ProcessMessages;

  I := 1;
  While (I < PageCount) and not Printer.Aborted do
    Begin
      If not Printer.Aborted Then
        Printer.NewPage;
      If not Printer.Aborted Then
        Printer.Canvas.Draw(0, 0, Pages[I]);
      Inc(I);
      If Assigned(FOnPrintProgress) Then
        FOnPrintProgress(Self, I, PageCount);
      Application.ProcessMessages;
    End;

  If not Printer.Aborted Then
    Printer.EndDoc;
end;

function TJvTFPrinter.PrinterToScreen(Value: Integer;
  Horizontal: Boolean): Integer;
var
  ScreenPPI,
  PrinterPPI : Integer;
begin
  ScreenPPI := Screen.PixelsPerInch;
  If Horizontal Then
    PrinterPPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSX)
  Else
    PrinterPPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSY);

  Result := Trunc(ScreenPPI / PrinterPPI * Value);
end;

procedure TJvTFPrinter.SaveDocToFiles(BaseFileName: TFileName);
var
  I : Integer;
begin
  If State <> spsFinished Then
    Raise EJvTFPrinterError.Create('Document must be Finished to save to file');

  For I := 0 to PageCount - 1 do
    Pages[I].SaveToFile(BaseFileName + '_' + IntToStr(I + 1) + '.emf');
end;

function TJvTFPrinter.ScreenToPrinter(Value: Integer;
  Horizontal: Boolean): Integer;
var
  ScreenPPI,
  PrinterPPI : Integer;
begin
  ScreenPPI := Screen.PixelsPerInch;
  If Horizontal Then
    Begin
      PrinterPPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSX);
    End
  Else
    Begin
      PrinterPPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSY);
    End;

  Result := Trunc(PrinterPPI / ScreenPPI * Value);
end;

procedure TJvTFPrinter.SetDirectPrint(Value: Boolean);
begin
  SetPropertyCheck;
  FDirectPrint := Value;
end;

procedure TJvTFPrinter.SetMarginOffset(Index, Value: Integer);
begin
  // Allow negative value...
  // SetMargin will catch that case and throw exception
  Case Index of
    1 : FMarginOffsets.Left := Value;
    2 : FMarginOffsets.Top := Value;
    3 : FMarginOffsets.Right := Value;
  Else
    FMarginOffsets.Bottom := Value;
  End;
end;

procedure TJvTFPrinter.SetMeasure(Value: TJvTFPrinterMeasure);
begin
  Try
    FConvertingProps := True;
    If Value <> FMeasure Then
      Begin
        PageLayout.FHeaderHeight := ConvertMeasure(PageLayout.FHeaderHeight,
          FMeasure, Value, False);
        PageLayout.FFooterHeight := ConvertMeasure(PageLayout.FFooterHeight,
          FMeasure, Value, False);

        PageLayout.FMargins.Left := ConvertMeasure(PageLayout.FMargins.Left,
          FMeasure, Value, True);
        PageLayout.FMargins.Right := ConvertMeasure(PageLayout.FMargins.Right,
          FMeasure, Value, True);
        PageLayout.FMargins.Top := ConvertMeasure(PageLayout.FMargins.Top,
          FMeasure, Value, False);
        PageLayout.FMargins.Bottom := ConvertMeasure(PageLayout.FMargins.Bottom,
          FMeasure, Value, False);
        FMeasure := Value;
      End;
  Finally
    FConvertingProps := False;
  End;
end;

procedure TJvTFPrinter.SetPageLayout(Value: TJvTFPrinterPageLayout);
begin
  FPageLayout.Assign(Value);
end;

procedure TJvTFPrinter.SetPropertyCheck;
begin
  If (State <> spsNoDoc) and not ConvertingProps Then
    Raise EJvTFPrinterError.Create('This property cannot be changed if a ' +
      'document exists');
end;

procedure TJvTFPrinter.SetTitle(Value: String);
begin
  FTitle := Value;
end;

class procedure TJvTFPrinter.Ver(var Major, Minor, Control : Word);
begin
  // utf can be purchased piece-meal or as a suite.  If purchased
  // (or upgraded) piece-by-piece the possibility of componet version
  // conflicts arises.  This internal versioning system will be used
  // to combat that problem.

  // ** PLEASE DO NOT CHANGE THE NUMBERS BELOW !!
  Major := 1;
  Minor := 4;
  Control := 1;
end;

class function TJvTFPrinter.VerStr : String;
var
  Major,
  Minor,
  Control : Word;
begin
  Ver(Major, Minor, Control);
  Result := IntToStr(Major) + '.' + IntToStr(Minor) + '.' + IntToStr(Control);
end;

{ TJvTFPrinterPageLayout }

procedure TJvTFPrinterPageLayout.Assign(Source: TPersistent);
var
  SourceMeas,
  DestMeas : TJvTFPrinterMeasure;
  WorkVal : Integer;
  SourceLayout : TJvTFPrinterPageLayout;
begin
  If (Source is TJvTFPrinterPageLayout) and Assigned(utfPrinter) and
    Assigned(TJvTFPrinterPageLayout(Source).utfPrinter) Then
    Begin
      SourceLayout := TJvTFPrinterPageLayout(Source);
      SourceMeas := SourceLayout.utfPrinter.Measure;
      DestMeas := utfPrinter.Measure;

      WorkVal := SourceLayout.MarginLeft;
      WorkVal := utfPrinter.ConvertMeasure(WorkVal, SourceMeas, DestMeas, True);
      SetMargin(1, WorkVal);

      WorkVal := SourceLayout.MarginTop;
      WorkVal := utfPrinter.ConvertMeasure(WorkVal, SourceMeas, DestMeas, False);
      SetMargin(2, WorkVal);

      WorkVal := SourceLayout.MarginRight;
      WorkVal := utfPrinter.ConvertMeasure(WorkVal, SourceMeas, DestMeas, True);
      SetMargin(3, WorkVal);

      WorkVal := SourceLayout.MarginBottom;
      WorkVal := utfPrinter.ConvertMeasure(WorkVal, SourceMeas, DestMeas, False);
      SetMargin(4, WorkVal);

      WorkVal := SourceLayout.HeaderHeight;
      WorkVal := utfPrinter.ConvertMeasure(WorkVal, SourceMeas, DestMeas, False);
      SetHeaderHeight(WorkVal);

      WorkVal := SourceLayout.FooterHeight;
      WorkVal := utfPrinter.ConvertMeasure(WorkVal, SourceMeas, DestMeas, False);
      SetFooterHeight(WorkVal);
    End
  Else
    Inherited Assign(Source);
end;

procedure TJvTFPrinterPageLayout.Change;
begin
  // do nothing, leave to descendants
end;

constructor TJvTFPrinterPageLayout.Create(autfPrinter: TJvTFPrinter);
begin
  inherited Create;
  If not Assigned(autfPrinter) Then
    Raise EJvTFPrinterError.Create('Could not create TJvTFPrinterPageLayout ' +
      'because autfPrinter must be assigned');

  FutfPrinter := autfPrinter;
end;

function TJvTFPrinterPageLayout.GetMargin(Index: Integer): Integer;
begin
  Case Index of
    1 : Result := FMargins.Left;
    2 : Result := FMargins.Top;
    3 : Result := FMargins.Right;
  Else
    Result := FMargins.Bottom;
  End;
end;

procedure TJvTFPrinterPageLayout.SetFooterHeight(Value: Integer);
var
  Check : Integer;
begin
  SetPropertyCheck;

  If Value < 0 Then
    Value := 0;

  If Value <> FFooterHeight Then
    Begin
      Check := FFooterHeight;
      FFooterHeight := Value;
      If utfPrinter.BodyHeight < 1 Then
        Begin
          FFooterHeight := Check;
          Raise EJvTFPrinterError.Create('Invalid Footer Height (' +
            IntToStr(Value) + ')');
        End
      Else
        Change;
    End;
end;

procedure TJvTFPrinterPageLayout.SetHeaderHeight(Value: Integer);
var
  Check : Integer;
begin
  SetPropertyCheck;

  If Value < 0 Then
    Value := 0;
  If Value <> FHeaderHeight Then
    Begin
      Check := FHeaderHeight;
      FHeaderHeight := Value;
      If utfPrinter.BodyHeight < 1 Then
        Begin
          FHeaderHeight := Check;
          Raise EJvTFPrinterError.Create('Invalid Header Height (' +
            IntToStr(Value) + ')');
        End
      Else
        Change;
    End;
end;

procedure TJvTFPrinterPageLayout.SetMargin(Index, Value: Integer);
var
  Unprintable,
  UserMarginPels,
  CurrMargin,
  NewMargin : Integer;
  Horz,
  Err : Boolean;
begin
  SetPropertyCheck;

  CurrMargin := GetMargin(Index);
  If Value <> CurrMargin Then
    Begin
      Horz := (Index = 1) or (Index = 3);
      Case Index Of
        1 : Unprintable := utfPrinter.GetUnprintable.Left;
        2 : Unprintable := utfPrinter.GetUnprintable.Top;
        3 : Unprintable := utfPrinter.GetUnprintable.Right;
      Else
        Unprintable := utfPrinter.GetUnprintable.Bottom;
      End;

      UserMarginPels := utfPrinter.ConvertMeasure(Value, utfPrinter.Measure,
        pmPixels, Horz);
      utfPrinter.SetMarginOffset(Index, UserMarginPels - Unprintable);

      If utfPrinter.GetMarginOffset(Index) >= 0 Then
        Begin
          Err := False;
          NewMargin := Value;
        End
      Else
        Begin
          Err := True;
          utfPrinter.SetMarginOffset(Index, 0);
          NewMargin := utfPrinter.ConvertMeasure(Unprintable, pmPixels,
            utfPrinter.Measure, Horz);
        End;

      If not Err Then
        Case Index of
          1 : FMargins.Left := NewMargin;
          2 : FMargins.Top := NewMargin;
          3 : FMargins.Right := NewMargin;
        Else
          FMargins.Bottom := NewMargin;
        End
      Else
        //SetMargin(Index, NewMargin);
        Case Index of
          1 : MarginLeft := NewMargin;
          2 : MarginTop := NewMargin;
          3 : MarginRight := NewMargin;
        Else
          MarginBottom := NewMargin;
        End;

      If Err and Assigned(utfPrinter) Then
        Begin
          utfPrinter.UpdateDesigner;
          utfPrinter.MarginError;
        End;

      Change;
    End;
end;

procedure TJvTFPrinterPageLayout.SetPropertyCheck;
begin
  utfPrinter.SetPropertyCheck;
end;



{ TJvTFUniversalPrinter }

procedure TJvTFUniversalPrinter.CreateDoc;
begin
  inherited;
end;

procedure TJvTFUniversalPrinter.FinishDoc;
begin
  inherited;
end;

procedure TJvTFUniversalPrinter.NewDoc;
begin
  inherited;
end;

procedure TJvTFUniversalPrinter.NewPage;
begin
  inherited;
end;

{ TJvTFHintProps }

procedure TJvTFHintProps.Assign(Source: TPersistent);
begin
  If Source is TJvTFHint Then
    Begin
      FHintColor := TJvTFHintProps(Source).HintColor;
      FHintHidePause := TJvTFHintProps(Source).HintHidePause;
      FHintPause := TJvTFHintProps(Source).HintPause;
      Change;
    End
  Else
    Inherited Assign(Source);
end;

procedure TJvTFHintProps.Change;
begin
  // do nothing
end;

constructor TJvTFHintProps.Create(aOwner: TJvTFControl);
begin
  Inherited Create;
  FutfControl := aOwner;

  FHintColor := clDefault;
  FHintHidePause := -1;
  FHintPause := -1;
end;

procedure TJvTFHintProps.SetHintColor(Value: TColor);
begin
  If Value <> FHintColor Then
    Begin
      FHintColor := Value;
      Change;
    End;
end;

procedure TJvTFHintProps.SetHintHidePause(Value: Integer);
begin
  If Value < -1 Then
    Value := -1;

  If Value <> FHintHidePause Then
    Begin
      FHintHidePause := Value;
      Change;
    End;
end;

procedure TJvTFHintProps.SetHintPause(Value: Integer);
begin
  If Value < -1 Then
    Value := -1;

  If Value <> HintPause Then
    Begin
      FHintPause := Value;
      Change;
    End;
end;

{ TJvTFDWNames }

procedure TJvTFDWNames.Assign(Source: TPersistent);
begin
  If Source is TJvTFDWNames Then
    Begin
      FDWN_Sunday    := TJvTFDWNames(Source).DWN_Sunday;
      FDWN_Monday    := TJvTFDWNames(Source).DWN_Monday;
      FDWN_Tuesday   := TJvTFDWNames(Source).DWN_Tuesday;
      FDWN_Wednesday := TJvTFDWNames(Source).DWN_Wednesday;
      FDWN_Thursday  := TJvTFDWNames(Source).DWN_Thursday;
      FDWN_Friday    := TJvTFDWNames(Source).DWN_Friday;
      FDWN_Saturday  := TJvTFDWNames(Source).DWN_Saturday;
      FSource := TJvTFDWNames(Source).Source;
      Change;
    End
  Else
    Inherited Assign(Source);
end;

procedure TJvTFDWNames.Change;
begin
  If Assigned(FOnChange) Then
    FOnChange(Self);
end;

constructor TJvTFDWNames.Create;
begin
  inherited;
  FSource := dwnsSysShort;
  FDWN_Sunday    := 'S';
  FDWN_Monday    := 'M';
  FDWN_Tuesday   := 'T';
  FDWN_Wednesday := 'W';
  FDWN_Thursday  := 'T';
  FDWN_Friday    := 'F';
  FDWN_Saturday  := 'S';
end;

function TJvTFDWNames.GetDWN(Index: Integer): String;
begin
  Case Index of
    1 : Result := FDWN_Sunday;
    2 : Result := FDWN_Monday;
    3 : Result := FDWN_Tuesday;
    4 : Result := FDWN_Wednesday;
    5 : Result := FDWN_Thursday;
    6 : Result := FDWN_Friday;
    7 : Result := FDWN_Saturday;
  Else
    Result := '';
  End;
end;

function TJvTFDWNames.GetDWName(DWIndex: Integer): String;
begin
  Case Source of
    dwnsSysLong : Result := SysUtils.LongDayNames[DWIndex];
    dwnsSysShort : Result := SysUtils.ShortDayNames[DWIndex];
  Else // dwnsCustom
    Result := GetDWN(DWIndex);
  End;
end;

procedure TJvTFDWNames.SetDWN(Index: Integer; Value: String);
begin
  Case Index of
    1 : FDWN_Sunday := Value;
    2 : FDWN_Monday := Value;
    3 : FDWN_Tuesday := Value;
    4 : FDWN_Wednesday := Value;
    5 : FDWN_Thursday := Value;
    6 : FDWN_Friday := Value;
    7 : FDWN_Saturday := Value;
  End;

  If Source = dwnsCustom Then
    Change;
end;

procedure TJvTFDWNames.SetSource(Value: TJvTFDWNameSource);
begin
  If Value <> FSource Then
    Begin
      FSource := Value;
      Change;
    End;
end;


{ TJvTFDateList }

function TJvTFDateList.Add(aDate: TDate): Integer;
begin
  Result := FList.Add(IntToStr(Trunc(aDate)));
  Change;
end;

procedure TJvTFDateList.Change;
begin
  If Assigned(FOnChange) Then
    FOnChange(Self);
end;

procedure TJvTFDateList.Clear;
begin
  FList.Clear;
  Change;
end;

function TJvTFDateList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TJvTFDateList.Create;
begin
  Inherited Create;
  FList := TStringList.Create;
  FList.Sorted := True;
  FList.Duplicates := dupIgnore;
end;

procedure TJvTFDateList.Delete(Index: Integer);
begin
  FList.Delete(Index);
  Change;
end;

destructor TJvTFDateList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TJvTFDateList.GetDate(Index: Integer): TDate;
begin
  Result := StrToInt(FList[Index]);
end;

function TJvTFDateList.IndexOf(aDate: TDate): Integer;
begin
  Result := FList.IndexOf(IntToStr(Trunc(aDate)));
end;

{ TJvTFNavigator }

//function TJvTFNavigator.ControlCount: Integer;
//begin
//  Result := FControls.Count;
//end;
//
//constructor TJvTFNavigator.Create(AOwner: TComponent);
//begin
//  inherited;
//  FControls := TStringList.Create;
//end;
//
//destructor TJvTFNavigator.Destroy;
//begin
//  While ControlCount > 0 do
//    UnregisterControl(Controls[0]);
//  FControls.Free;
//
//  inherited;
//end;
//
//function TJvTFNavigator.GetControl(Index: Integer): TJvTFControl;
//begin
//  Result := TJvTFControl(FControls.Objects[Index]);
//end;
//
//procedure TJvTFNavigator.Navigate(aControl: TJvTFControl;
//  SchedNames: TStringList; Dates: TJvTFDateList);
//var
//  I : Integer;
//  Control : TJvTFControl;
//begin
//  If Navigating or not Assigned(aControl) Then
//    Exit;
//
//  If Assigned(FBeforeNavigate) Then
//    FBeforeNavigate(Self, aControl, SchedNames, Dates);
//
//  FNavigating := True;
//  Try
//    For I := 0 to ControlCount - 1 do
//      Begin
//        Control := Controls[I];
//        If Control <> aControl Then
//          //Controls[I].Notify(aControl, sncNavigate);
//          Control.Navigate(aControl, SchedNames, Dates);
//      End;
//  Finally
//    FNavigating := False;
//  End;
//
//  If Assigned(FAfterNavigate) Then
//    FAfterNavigate(Self, aControl, SchedNames, Dates);
//end;
//
//procedure TJvTFNavigator.RegisterControl(aControl: TJvTFControl);
//var
//  I : Integer;
//begin
//  I := FControls.IndexOfObject(aControl);
//  If I = -1 Then
//    FControls.AddObject('', aControl);
//end;
//
//procedure TJvTFNavigator.UnregisterControl(aControl: TJvTFControl);
//var
//  I : Integer;
//begin
//  I := FControls.IndexOfObject(aControl);
//  If I > -1 Then
//    FControls.Delete(I);
//end;

Initialization
  Randomize;

end.




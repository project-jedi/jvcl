unit eventtypes;

interface

type

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // mxgraph.pas
  //

  // <EXTLINK borland://TDimEvent>Click here to view a description of this event.</EXTLINK>
  TDimEvent = procedure (Sender: TObject; iDim: Integer) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // mxstore.pas
  //

  // <EXTLINK borland://TCapacityErrorEvent>Click here to view a description of this event.</EXTLINK>
  TCapacityErrorEvent = procedure(var EAction: TErrorAction) of object;
  // <EXTLINK borland://TCubeNotifyEvent>Click here to view a description of this event.</EXTLINK>
  TCubeNotifyEvent  = procedure(DataCube: TCustomDataStore) of object;
  // <EXTLINK borland://TCubeRefreshEvent>Click here to view a description of this event.</EXTLINK>
  TCubeRefreshEvent = procedure(DataCube: TCustomDataStore; DimMap: TCubeDims) of object;
  // <EXTLINK borland://TCubeDimTransformEvent>Click here to view a description of this event.</EXTLINK>
  TCubeDimTransformEvent = procedure(var Value: Variant; Data: TCubeDim) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // dbweb.pas
  //

  // <EXTLINK borland://TCreateContentEvent>Click here to view a description of this event.</EXTLINK>
  TCreateContentEvent = procedure (Sender: TObject; var Continue: Boolean) of object;
  // <EXTLINK borland://THTMLGetTableCaptionEvent>Click here to view a description of this event.</EXTLINK>
  THTMLGetTableCaptionEvent = procedure (Sender: TObject; var Caption: string; var Alignment: THTMLCaptionAlignment) of object;
  // <EXTLINK borland://THTMLFormatCellEvent>Click here to view a description of this event.</EXTLINK>
  THTMLFormatCellEvent = procedure (Sender: TObject; CellRow, CellColumn: Integer; var BgColor: THTMLBgColor; var Align: THTMLAlign; var VAlign: THTMLVAlign; var CustomAttrs, CellData: string) of object;
  // <EXTLINK borland://THTMLDataSetEmpty>Click here to view a description of this event.</EXTLINK>
  THTMLDataSetEmpty = procedure (Sender: TObject; var Continue: Boolean) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // midprod.pas
  //

  // <EXTLINK borland://TXMLDataEvent>Click here to view a description of this event.</EXTLINK>
  TXMLDataEvent = procedure (Sender: TObject; Request: TWebRequest; XMLBroker: TXMLBroker; var OwnerData: OleVariant) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // sqledit.pas
  //

  // <EXTLINK borland://TGetTableNamesProc>Click here to view a description of this event.</EXTLINK>
  TGetTableNamesProc = procedure(List: TStrings; SystemTables: Boolean) of object;
  // <EXTLINK borland://TGetFieldNamesProc>Click here to view a description of this event.</EXTLINK>
  TGetFieldNamesProc = procedure(const TableName: string; List: TStrings) of Object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // corbaobj.pas
  //

  // <EXTLINK borland://TUnmarshalProc>Click here to view a description of this event.</EXTLINK>
  TUnmarshalProc =  procedure (const Strm: IMarshalInBuffer; Cookie: Pointer) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // orbpas
  //

  // <EXTLINK borland://TCopyUserExceptionProc>Click here to view a description of this event.</EXTLINK>
  TCopyUserExceptionProc = procedure (const InBuf: IMarshalInBuffer) of object; register;
  // <EXTLINK borland://TThrowUserExceptionProc>Click here to view a description of this event.</EXTLINK>
  TThrowUserExceptionProc = procedure of object; register;
  // <EXTLINK borland://TUserExceptionFactoryProc>Click here to view a description of this event.</EXTLINK>
  TUserExceptionFactoryProc = function : PUserExceptionProxy; cdecl;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // comobj.pas
  //

  // <EXTLINK borland://TFactoryProc>Click here to view a description of this event.</EXTLINK>
  TFactoryProc = procedure(Factory: TComObjectFactory) of object;
  // <EXTLINK borland://TConnectEvent>Click here to view a description of this event.</EXTLINK>
  TConnectEvent = procedure (const Sink: IUnknown; Connecting: Boolean) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // comserv.pas
  //

  // <EXTLINK borland://TLastReleaseEvent>Click here to view a description of this event.</EXTLINK>
  TLastReleaseEvent = procedure(var Shutdown: Boolean) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // dsgnintf.pas
  //

  // <EXTLINK borland://TGetPropEditProc>Click here to view a description of this event.</EXTLINK>
  TGetPropEditProc = procedure(Prop: TPropertyEditor) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // toolintf.pas
  //

  // <EXTLINK borland://TIMenuClickEvent>Click here to view a description of this event.</EXTLINK>
  TIMenuClickEvent = procedure (Sender: TIMenuItemIntf) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // actnlist.pas
  //

  // <EXTLINK borland://TActionEvent>Click here to view a description of this event.</EXTLINK>
  TActionEvent = procedure (Action: TBasicAction; var Handled: Boolean) of object;
  // <EXTLINK borland://THintEvent>Click here to view a description of this event.</EXTLINK>
  THintEvent = procedure (var HintStr: string; var CanShow: Boolean) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // axctrls.pas
  //

  // <EXTLINK borland://TDefinePropertyPage>Click here to view a description of this event.</EXTLINK>
  TDefinePropertyPage = procedure(const GUID: TGUID) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // classes.pas
  //

  // <EXTLINK borland:/TNotifyEvent/TNotifyEvent>Click here to view a description of this event.</EXTLINK>
  TNotifyEvent = procedure(Sender: TObject) of object;
  // <EXTLINK borland://THelpEvent>Click here to view a description of this event.</EXTLINK>
  THelpEvent = function (Command: Word; Data: Longint; var CallHelp: Boolean): Boolean of object;
  // <EXTLINK borland://TGetStrProc>Click here to view a description of this event.</EXTLINK>
  TGetStrProc = procedure(const S: string) of object;
  // <EXTLINK borland://TReaderProc>Click here to view a description of this event.</EXTLINK>
  TReaderProc = procedure(Reader: TReader) of object;
  // <EXTLINK borland://TWriterProc>Click here to view a description of this event.</EXTLINK>
  TWriterProc = procedure(Writer: TWriter) of object;
  // <EXTLINK borland://TStreamProc>Click here to view a description of this event.</EXTLINK>
  TStreamProc = procedure(Stream: TStream) of object;
  // <EXTLINK borland://TFindMethodEvent>Click here to view a description of this event.</EXTLINK>
  TFindMethodEvent = procedure(Reader: TReader; const MethodName: string; var Address: Pointer; var Error: Boolean) of object;
  // <EXTLINK borland://TSetNameEvent>Click here to view a description of this event.</EXTLINK>
  TSetNameEvent = procedure(Reader: TReader; Component: TComponent; var Name: string) of object;
  // <EXTLINK borland://TReferenceNameEvent>Click here to view a description of this event.</EXTLINK>
  TReferenceNameEvent = procedure(Reader: TReader; var Name: string) of object;
  // <EXTLINK borland://TAncestorNotFoundEvent>Click here to view a description of this event.</EXTLINK>
  TAncestorNotFoundEvent = procedure(Reader: TReader; const ComponentName: string; ComponentClass: TPersistentClass; var Component: TComponent) of object;
  // <EXTLINK borland://TReadComponentsProc>Click here to view a description of this event.</EXTLINK>
  TReadComponentsProc = procedure(Component: TComponent) of object;
  // <EXTLINK borland://TReaderError>Click here to view a description of this event.</EXTLINK>
  TReaderError = procedure(Reader: TReader; const Message: string; var Handled: Boolean) of object;
  // <EXTLINK borland://TFindComponentClassEvent>Click here to view a description of this event.</EXTLINK>
  TFindComponentClassEvent = procedure(Reader: TReader; const ClassName: string; var ComponentClass: TComponentClass) of object;
  // <EXTLINK borland://TCreateComponentEvent>Click here to view a description of this event.</EXTLINK>
  TCreateComponentEvent = procedure(Reader: TReader; ComponentClass: TComponentClass; var Component: TComponent) of object;
  // <EXTLINK borland://TGetChildProc>Click here to view a description of this event.</EXTLINK>
  TGetChildProc = procedure (Child: TComponent) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // comctrls.pas
  //

  // <EXTLINK borland://TTVChangingEvent>Click here to view a description of this event.</EXTLINK>
  TTVChangingEvent = procedure(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean) of object;
  // <EXTLINK borland://TTVChangedEvent>Click here to view a description of this event.</EXTLINK>
  TTVChangedEvent = procedure(Sender: TObject; Node: TTreeNode) of object;
  // <EXTLINK borland://TTVEditingEvent>Click here to view a description of this event.</EXTLINK>
  TTVEditingEvent = procedure(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean) of object;
  // <EXTLINK borland://TTVEditedEvent>Click here to view a description of this event.</EXTLINK>
  TTVEditedEvent = procedure(Sender: TObject; Node: TTreeNode; var S: string) of object;
  // <EXTLINK borland://TTVExpandingEvent>Click here to view a description of this event.</EXTLINK>
  TTVExpandingEvent = procedure(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean) of object;
  // <EXTLINK borland://TTVCollapsingEvent>Click here to view a description of this event.</EXTLINK>
  TTVCollapsingEvent = procedure(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean) of object;
  // <EXTLINK borland://TTVExpandedEvent>Click here to view a description of this event.</EXTLINK>
  TTVExpandedEvent = procedure(Sender: TObject; Node: TTreeNode) of object;
  // <EXTLINK borland://TTVCompareEvent>Click here to view a description of this event.</EXTLINK>
  TTVCompareEvent = procedure(Sender: TObject; Node1, Node2: TTreeNode; Data: Integer; var Compare: Integer) of object;
  // <EXTLINK borland://TTVCustomDrawEvent>Click here to view a description of this event.</EXTLINK>
  TTVCustomDrawEvent = procedure(Sender: TCustomTreeView; const ARect: TRect; var DefaultDraw: Boolean) of object;
  // <EXTLINK borland://TTVCustomDrawItemEvent>Click here to view a description of this event.</EXTLINK>
  TTVCustomDrawItemEvent = procedure(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean) of object;
  // <EXTLINK borland://TTVAdvancedCustomDrawEvent>Click here to view a description of this event.</EXTLINK>
  TTVAdvancedCustomDrawEvent = procedure(Sender: TCustomTreeView; const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;
  // <EXTLINK borland://TTVAdvancedCustomDrawItemEvent>Click here to view a description of this event.</EXTLINK>
  TTVAdvancedCustomDrawItemEvent = procedure(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean) of object;

  // <EXTLINK borland://TRichEditResizeEvent>Click here to view a description of this event.</EXTLINK>
  TRichEditResizeEvent = procedure(Sender: TObject; Rect: TRect) of object;
  // <EXTLINK borland://TRichEditProtectChange>Click here to view a description of this event.</EXTLINK>
  TRichEditProtectChange = procedure(Sender: TObject; StartPos, EndPos: Integer; var AllowChange: Boolean) of object;
  // <EXTLINK borland://TRichEditSaveClipboard>Click here to view a description of this event.</EXTLINK>
  TRichEditSaveClipboard = procedure(Sender: TObject; NumObjects, NumChars: Integer; var SaveClipboard: Boolean) of object;

  // <EXTLINK borland://TUDClickEvent>Click here to view a description of this event.</EXTLINK>
  TUDClickEvent = procedure (Sender: TObject; Button: TUDBtnType) of object;
  // <EXTLINK borland://TUDChangingEvent>Click here to view a description of this event.</EXTLINK>
  TUDChangingEvent = procedure (Sender: TObject; var AllowChange: Boolean) of object;
  // <EXTLINK borland://TUDChangingEventEx>Click here to view a description of this event.</EXTLINK>
  TUDChangingEventEx = procedure (Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection) of object;

  // <EXTLINK borland://TLVDeletedEvent>Click here to view a description of this event.</EXTLINK>
  TLVDeletedEvent = procedure(Sender: TObject; Item: TListItem) of object;
  // <EXTLINK borland://TLVEditingEvent>Click here to view a description of this event.</EXTLINK>
  TLVEditingEvent = procedure(Sender: TObject; Item: TListItem; var AllowEdit: Boolean) of object;
  // <EXTLINK borland://TLVEditedEvent>Click here to view a description of this event.</EXTLINK>
  TLVEditedEvent = procedure(Sender: TObject; Item: TListItem; var S: string) of object;
  // <EXTLINK borland://TLVChangeEvent>Click here to view a description of this event.</EXTLINK>
  TLVChangeEvent = procedure(Sender: TObject; Item: TListItem; Change: TItemChange) of object;
  // <EXTLINK borland://TLVChangingEvent>Click here to view a description of this event.</EXTLINK>
  TLVChangingEvent = procedure(Sender: TObject; Item: TListItem; Change: TItemChange; var AllowChange: Boolean) of object;
  // <EXTLINK borland://TLVColumnClickEvent>Click here to view a description of this event.</EXTLINK>
  TLVColumnClickEvent = procedure(Sender: TObject; Column: TListColumn) of object;
  // <EXTLINK borland://TLVColumnRClickEvent>Click here to view a description of this event.</EXTLINK>
  TLVColumnRClickEvent = procedure(Sender: TObject; Column: TListColumn; Point: TPoint) of object;
  // <EXTLINK borland://TLVCompareEvent>Click here to view a description of this event.</EXTLINK>
  TLVCompareEvent = procedure(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer) of object;
  // <EXTLINK borland://TLVNotifyEvent>Click here to view a description of this event.</EXTLINK>
  TLVNotifyEvent = procedure(Sender: TObject; Item: TListItem) of object;
  // <EXTLINK borland://TLVSelectItemEvent>Click here to view a description of this event.</EXTLINK>
  TLVSelectItemEvent = procedure(Sender: TObject; Item: TListItem; Selected: Boolean) of object;
  // <EXTLINK borland://TLVDrawItemEvent>Click here to view a description of this event.</EXTLINK>
  TLVDrawItemEvent = procedure(Sender: TCustomListView; Item: TListItem; Rect: TRect; State: TOwnerDrawState) of object;
  // <EXTLINK borland://TLVCustomDrawEvent>Click here to view a description of this event.</EXTLINK>
  TLVCustomDrawEvent = procedure(Sender: TCustomListView; const ARect: TRect; var DefaultDraw: Boolean) of object;
  // <EXTLINK borland://TLVCustomDrawItemEvent>Click here to view a description of this event.</EXTLINK>
  TLVCustomDrawItemEvent = procedure(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean) of object;
  // <EXTLINK borland://TLVCustomDrawSubItemEvent>Click here to view a description of this event.</EXTLINK>
  TLVCustomDrawSubItemEvent = procedure(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean) of object;
  // <EXTLINK borland://TLVAdvancedCustomDrawEvent>Click here to view a description of this event.</EXTLINK>
  TLVAdvancedCustomDrawEvent = procedure(Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;
  // <EXTLINK borland://TLVAdvancedCustomDrawItemEvent>Click here to view a description of this event.</EXTLINK>
  TLVAdvancedCustomDrawItemEvent = procedure(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;
  // <EXTLINK borland://TLVAdvancedCustomDrawSubItemEvent>Click here to view a description of this event.</EXTLINK>
  TLVAdvancedCustomDrawSubItemEvent = procedure(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;
  // <EXTLINK borland://TLVOwnerDataEvent>Click here to view a description of this event.</EXTLINK>
  TLVOwnerDataEvent = procedure(Sender: TObject; Item: TListItem) of object;
  // <EXTLINK borland://TLVOwnerDataFindEvent>Click here to view a description of this event.</EXTLINK>
  TLVOwnerDataFindEvent = procedure(Sender: TObject; Find: TItemFind; const FindString: string; const FindPosition: TPoint; FindData: Pointer; StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean; var Index: Integer) of object;
  // <EXTLINK borland://TLVOwnerDataHintEvent>Click here to view a description of this event.</EXTLINK>
  TLVOwnerDataHintEvent = procedure(Sender: TObject; StartIndex, EndIndex: Integer) of object;
  // <EXTLINK borland://TLVOwnerDataStateChangeEvent>Click here to view a description of this event.</EXTLINK>
  TLVOwnerDataStateChangeEvent = procedure(Sender: TObject; StartIndex, EndIndex: Integer; OldState, NewState: TItemStates) of object;
  // <EXTLINK borland://TLVSubItemImageEvent>Click here to view a description of this event.</EXTLINK>
  TLVSubItemImageEvent = procedure(Sender: TObject; Item: TListItem; SubItem: Integer; var ImageIndex: Integer) of object;
  // <EXTLINK borland://TLVInfoTipEvent>Click here to view a description of this event.</EXTLINK>
  TLVInfoTipEvent = procedure(Sender: TObject; Item: TListItem; var InfoTip: string) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // contnrs.pas
  //

  // <EXTLINK borland://TComponentListNexusEvent>Click here to view a description of this event.</EXTLINK>
  TComponentListNexusEvent = procedure(Sender: TObject; AComponent: TComponent) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // controls
  //

  // <EXTLINK borland://TMouseEvent>Click here to view a description of this event.</EXTLINK>
  TMouseEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  // <EXTLINK borland://TMouseMoveEvent>Click here to view a description of this event.</EXTLINK>
  TMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer) of object;
  // <EXTLINK borland://TKeyEvent>Click here to view a description of this event.</EXTLINK>
  TKeyEvent = procedure(Sender: TObject; var Key: Word; Shift: TShiftState) of object;
  // <EXTLINK borland://TKeyPressEvent>Click here to view a description of this event.</EXTLINK>
  TKeyPressEvent = procedure(Sender: TObject; var Key: Char) of object;
  // <EXTLINK borland://TDragOverEvent>Click here to view a description of this event.</EXTLINK>
  TDragOverEvent = procedure(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean) of object;
  // <EXTLINK borland://TDragDropEvent>Click here to view a description of this event.</EXTLINK>
  TDragDropEvent = procedure(Sender, Source: TObject; X, Y: Integer) of object;
  // <EXTLINK borland://TStartDragEvent>Click here to view a description of this event.</EXTLINK>
  TStartDragEvent = procedure(Sender: TObject; var DragObject: TDragObject) of object;
  // <EXTLINK borland://TEndDragEvent>Click here to view a description of this event.</EXTLINK>
  TEndDragEvent = procedure(Sender, Target: TObject; X, Y: Integer) of object;
  // <EXTLINK borland://TDockDropEvent>Click here to view a description of this event.</EXTLINK>
  TDockDropEvent = procedure(Sender: TObject; Source: TDragDockObject; X, Y: Integer) of object;
  // <EXTLINK borland://TDockOverEvent>Click here to view a description of this event.</EXTLINK>
  TDockOverEvent = procedure(Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean) of object;
  // <EXTLINK borland://TUnDockEvent>Click here to view a description of this event.</EXTLINK>
  TUnDockEvent = procedure(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean) of object;
  // <EXTLINK borland://TStartDockEvent>Click here to view a description of this event.</EXTLINK>
  TStartDockEvent = procedure(Sender: TObject; var DragObject: TDragDockObject) of object;
  // <EXTLINK borland://TGetSiteInfoEvent>Click here to view a description of this event.</EXTLINK>
  TGetSiteInfoEvent = procedure(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean) of object;
  // <EXTLINK borland://TCanResizeEvent>Click here to view a description of this event.</EXTLINK>
  TCanResizeEvent = procedure(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean) of object;
  // <EXTLINK borland://TConstrainedResizeEvent>Click here to view a description of this event.</EXTLINK>
  TConstrainedResizeEvent = procedure(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer) of object;
  // <EXTLINK borland://TMouseWheelEvent>Click here to view a description of this event.</EXTLINK>
  TMouseWheelEvent = procedure(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean) of object;
  // <EXTLINK borland://TMouseWheelUpDownEvent>Click here to view a description of this event.</EXTLINK>
  TMouseWheelUpDownEvent = procedure(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean) of object;
  // <EXTLINK borland://TContextPopupEvent>Click here to view a description of this event.</EXTLINK>
  TContextPopupEvent = procedure(Sender: TObject; MousePos: TPoint; var Handled: Boolean) of object;

  // <EXTLINK borland://TWndMethod>Click here to view a description of this event.</EXTLINK>
  TWndMethod = procedure(var Message: TMessage) of object;

  // <EXTLINK borland://TForEachZoneProc>Click here to view a description of this event.</EXTLINK>
  TForEachZoneProc = procedure(Zone: TDockZone) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // corbacon.pas
  //

  // <EXTLINK borland://TTimedOutEvent>Click here to view a description of this event.</EXTLINK>
  TTimedOutEvent = procedure (var Msg: string; var Cancel: Boolean) of object;


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // ctlpanel.pas
  //

  // <EXTLINK borland://TInitEvent>Click here to view a description of this event.</EXTLINK>
  TInitEvent = procedure (Sender: TObject; var AppInitOK: Boolean) of object;
  // <EXTLINK borland://TCountEvent>Click here to view a description of this event.</EXTLINK>
  TCountEvent = procedure (Sender: TObject; var AppCount: Integer) of object;
  // <EXTLINK borland://TExitEvent>Click here to view a description of this event.</EXTLINK>
  TExitEvent = TNotifyEvent;
  // <EXTLINK borland://TSetupEvent>Click here to view a description of this event.</EXTLINK>
  TSetupEvent = TNotifyEvent;

  // <EXTLINK borland://TActivateEvent>Click here to view a description of this event.</EXTLINK>
  TActivateEvent     = procedure (Sender: TObject; Data: LongInt) of object;
  // <EXTLINK borland://TStopEvent>Click here to view a description of this event.</EXTLINK>
  TStopEvent         = procedure (Sender: TObject; Data: LongInt) of object;
  // <EXTLINK borland://TInquireEvent>Click here to view a description of this event.</EXTLINK>
  TInquireEvent      = procedure (Sender: TObject; var idIcon: Integer; var idName: Integer; var idInfo: Integer; var lData: Integer) of object;
  // <EXTLINK borland://TNewInquireEvent>Click here to view a description of this event.</EXTLINK>
  TNewInquireEvent   = procedure (Sender: TObject; var lData: Integer; var hIcon: HICON; var AppletName: string; var AppletInfo: string) of object;
  // <EXTLINK borland://TStartWParmsEvent>Click here to view a description of this event.</EXTLINK>
  TStartWParmsEvent  = procedure (Sender: TObject; Params: string) of object;


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // db
  //

  // <EXTLINK borland://TLoginEvent>Click here to view a description of this event.</EXTLINK>
  TLoginEvent = procedure(Sender: TObject; Username, Password: string) of object;
  // <EXTLINK borland://TConnectChangeEvent>Click here to view a description of this event.</EXTLINK>
  TConnectChangeEvent = procedure(Sender: TObject; Connecting: Boolean) of object;
  // <EXTLINK borland://TFieldNotifyEvent>Click here to view a description of this event.</EXTLINK>
  TFieldNotifyEvent = procedure(Sender: TField) of object;
  // <EXTLINK borland://TFieldGetTextEvent>Click here to view a description of this event.</EXTLINK>
  TFieldGetTextEvent = procedure(Sender: TField; var Text: string; DisplayText: Boolean) of object;
  // <EXTLINK borland://TFieldSetTextEvent>Click here to view a description of this event.</EXTLINK>
  TFieldSetTextEvent = procedure(Sender: TField; const Text: string) of object;
  // <EXTLINK borland://TDataChangeEvent>Click here to view a description of this event.</EXTLINK>
  TDataChangeEvent = procedure(Sender: TObject; Field: TField) of object;
  // <EXTLINK borland://TDataSetNotifyEvent>Click here to view a description of this event.</EXTLINK>
  TDataSetNotifyEvent = procedure(DataSet: TDataSet) of object;
  // <EXTLINK borland://TDataSetErrorEvent>Click here to view a description of this event.</EXTLINK>
  TDataSetErrorEvent = procedure(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction) of object;

  // <EXTLINK borland://TFilterRecordEvent>Click here to view a description of this event.</EXTLINK>
  TFilterRecordEvent = procedure(DataSet: TDataSet; var Accept: Boolean) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // dbclient
  //

  // <EXTLINK borland://TAggUpdateEvent>Click here to view a description of this event.</EXTLINK>
  TAggUpdateEvent = procedure(Agg: TAggregate) of object;
  // <EXTLINK borland://TReconcileErrorEvent>Click here to view a description of this event.</EXTLINK>
  TReconcileErrorEvent = procedure(DataSet: TClientDataSet; E: EReconcileError; UpdateKind: TUpdateKind; var Action: TReconcileAction) of object;
  // <EXTLINK borland://TRemoteEvent>Click here to view a description of this event.</EXTLINK>
  TRemoteEvent = procedure(Sender: TObject; var OwnerData: OleVariant) of object;
  // <EXTLINK borland://ENavClick>Click here to view a description of this event.</EXTLINK>
  ENavClick = procedure (Sender: TObject; Button: TNavigateBtn) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // dbgrids
  //

  // <EXTLINK borland://TDrawColumnCellEvent>Click here to view a description of this event.</EXTLINK>
  TDrawColumnCellEvent = procedure (Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState) of object;
  // <EXTLINK borland://TDBGridClickEvent>Click here to view a description of this event.</EXTLINK>
  TDBGridClickEvent = procedure (Column: TColumn) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // dbtables
  //

  // <EXTLINK borland://TPasswordEvent>Click here to view a description of this event.</EXTLINK>
  TPasswordEvent = procedure(Sender: TObject; var Continue: Boolean) of Object;
  // <EXTLINK borland://TDatabaseNotifyEvent>Click here to view a description of this event.</EXTLINK>
  TDatabaseNotifyEvent = procedure(DBEvent: TDatabaseEvent; const Param) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // ddeman
  //

  // <EXTLINK borland://TMacroEvent>Click here to view a description of this event.</EXTLINK>
  TMacroEvent = procedure(Sender: TObject; Msg: TStrings) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // dialogs
  //

  // <EXTLINK borland://TIncludeItemEvent>Click here to view a description of this event.</EXTLINK>
  TIncludeItemEvent = procedure (const OFN: TOFNotifyEx; var Include: Boolean) of object;
  // <EXTLINK borland://TFDApplyEvent>Click here to view a description of this event.</EXTLINK>
  TFDApplyEvent = procedure(Sender: TObject; Wnd: HWND) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // forms
  //

  // <EXTLINK borland://TCloseEvent>Click here to view a description of this event.</EXTLINK>
  TCloseEvent = procedure(Sender: TObject; var Action: TCloseAction) of object;
  // <EXTLINK borland://TCloseQueryEvent>Click here to view a description of this event.</EXTLINK>
  TCloseQueryEvent = procedure(Sender: TObject; var CanClose: Boolean) of object;
  // <EXTLINK borland://TShortCutEvent>Click here to view a description of this event.</EXTLINK>
  TShortCutEvent = procedure (var Msg: TWMKey; var Handled: Boolean) of object;
  // <EXTLINK borland://TMessageEvent>Click here to view a description of this event.</EXTLINK>
  TMessageEvent = procedure (var Msg: TMsg; var Handled: Boolean) of object;
  // <EXTLINK borland://TExceptionEvent>Click here to view a description of this event.</EXTLINK>
  TExceptionEvent = procedure (Sender: TObject; E: Exception) of object;
  // <EXTLINK borland://TIdleEvent>Click here to view a description of this event.</EXTLINK>
  TIdleEvent = procedure (Sender: TObject; var Done: Boolean) of object;
  // <EXTLINK borland://TShowHintEvent>Click here to view a description of this event.</EXTLINK>
  TShowHintEvent = procedure (var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo) of object;
  // <EXTLINK borland://TWindowHook>Click here to view a description of this event.</EXTLINK>
  TWindowHook = function (var Message: TMessage): Boolean of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // grids
  //

  // <EXTLINK borland://TGetEditEvent>Click here to view a description of this event.</EXTLINK>
  TGetEditEvent = procedure (Sender: TObject; ACol, ARow: Longint; var Value: string) of object;
  // <EXTLINK borland://TSetEditEvent>Click here to view a description of this event.</EXTLINK>
  TSetEditEvent = procedure (Sender: TObject; ACol, ARow: Longint; const Value: string) of object;
  // <EXTLINK borland://TMovedEvent>Click here to view a description of this event.</EXTLINK>
  TMovedEvent = procedure (Sender: TObject; FromIndex, ToIndex: Longint) of object;
  // <EXTLINK borland://TDrawCellEvent>Click here to view a description of this event.</EXTLINK>
  TDrawCellEvent = procedure (Sender: TObject; ACol, ARow: Longint;
    Rect: TRect; State: TGridDrawState) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // ibsqlmonitor
  //

  // <EXTLINK borland://TSQLEvent>Click here to view a description of this event.</EXTLINK>
  TSQLEvent = procedure(EventText: String) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // mconnect
  //

  // <EXTLINK borland://TGetUsernameEvent>Click here to view a description of this event.</EXTLINK>
  TGetUsernameEvent = procedure(Sender: TObject; var Username: string) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // menus
  //

  // <EXTLINK borland://TMenuChangeEvent>Click here to view a description of this event.</EXTLINK>
  TMenuChangeEvent = procedure (Sender: TObject; Source: TMenuItem; Rebuild: Boolean) of object;
  // <EXTLINK borland://TMenuDrawItemEvent>Click here to view a description of this event.</EXTLINK>
  TMenuDrawItemEvent = procedure (Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean) of object;
  // <EXTLINK borland://TAdvancedMenuDrawItemEvent>Click here to view a description of this event.</EXTLINK>
  TAdvancedMenuDrawItemEvent = procedure (Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState) of object;
  // <EXTLINK borland://TMenuMeasureItemEvent>Click here to view a description of this event.</EXTLINK>
  TMenuMeasureItemEvent = procedure (Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // mplayer
  //

  // <EXTLINK borland://EMPPostNotify>Click here to view a description of this event.</EXTLINK>
  EMPPostNotify = procedure (Sender: TObject; Button: TMPBtnType) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // oleauto
  //

  // <EXTLINK borland://TLastReleaseEvent>Click here to view a description of this event.</EXTLINK>
  TLastReleaseEvent = procedure(var Shutdown: Boolean) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // provider
  //

  // <EXTLINK borland://TPutFieldProc>Click here to view a description of this event.</EXTLINK>
  TPutFieldProc = procedure(Info: PPutFieldInfo) of object;
  // <EXTLINK borland://TGetParamsEvent>Click here to view a description of this event.</EXTLINK>
  TGetParamsEvent = procedure(DataSet: TDataSet; Params: TList) of object;
  // <EXTLINK borland://TProviderDataEvent>Click here to view a description of this event.</EXTLINK>
  TProviderDataEvent = procedure(Sender: TObject; DataSet: TClientDataSet) of object;
  // <EXTLINK borland://TBeforeUpdateRecordEvent>Click here to view a description of this event.</EXTLINK>
  TBeforeUpdateRecordEvent = procedure(Sender: TObject; SourceDS: TDataSet; DeltaDS: TClientDataSet; UpdateKind: TUpdateKind; var Applied: Boolean) of object;
  // <EXTLINK borland://TAfterUpdateRecordEvent>Click here to view a description of this event.</EXTLINK>
  TAfterUpdateRecordEvent = procedure(Sender: TObject; SourceDS: TDataSet; DeltaDS: TClientDataSet; UpdateKind: TUpdateKind) of object;
  // <EXTLINK borland://TResolverErrorEvent>Click here to view a description of this event.</EXTLINK>
  TResolverErrorEvent = procedure(Sender: TObject; DataSet: TClientDataSet; E: EUpdateError; UpdateKind: TUpdateKind; var Response: TResolverResponse) of object;


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // scktComp
  //

  // <EXTLINK borland://TSocketEventEvent>Click here to view a description of this event.</EXTLINK>
  TSocketEventEvent = procedure (Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent) of object;
  // <EXTLINK borland://TSocketErrorEvent>Click here to view a description of this event.</EXTLINK>
  TSocketErrorEvent = procedure (Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer) of object;
  // <EXTLINK borland://TGetSocketEvent>Click here to view a description of this event.</EXTLINK>
  TGetSocketEvent = procedure (Sender: TObject; Socket: TSocket; var ClientSocket: TServerClientWinSocket) of object;
  // <EXTLINK borland://TGetThreadEvent>Click here to view a description of this event.</EXTLINK>
  TGetThreadEvent = procedure (Sender: TObject; ClientSocket: TServerClientWinSocket; var SocketThread: TServerClientThread) of object;
  // <EXTLINK borland://TSocketNotifyEvent>Click here to view a description of this event.</EXTLINK>
  TSocketNotifyEvent = procedure (Sender: TObject; Socket: TCustomWinSocket) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // scktmain
  //

  // <EXTLINK borland://TSocketProc>Click here to view a description of this event.</EXTLINK>
  TSocketProc = procedure(Item: TListItem; Socket: TCustomWinSocket) of Object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // svcmgr
  //

  // <EXTLINK borland://TServiceEvent>Click here to view a description of this event.</EXTLINK>
  TServiceEvent = procedure(Sender: TService) of object;
  // <EXTLINK borland://TContinueEvent>Click here to view a description of this event.</EXTLINK>
  TContinueEvent = procedure(Sender: TService; var Continued: Boolean) of object;
  // <EXTLINK borland://TPauseEvent>Click here to view a description of this event.</EXTLINK>
  TPauseEvent = procedure(Sender: TService; var Paused: Boolean) of object;
  // <EXTLINK borland://TStartEvent>Click here to view a description of this event.</EXTLINK>
  TStartEvent = procedure(Sender: TService; var Started: Boolean) of object;
  // <EXTLINK borland://TStopEvent>Click here to view a description of this event.</EXTLINK>
  TStopEvent = procedure(Sender: TService; var Stopped: Boolean) of object;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // typeinfo
  //

  // <EXTLINK borland://TPropInfoProc>Click here to view a description of this event.</EXTLINK>
  TPropInfoProc = procedure(PropInfo: PPropInfo) of object;

implementation

end.
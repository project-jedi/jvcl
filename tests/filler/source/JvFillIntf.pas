unit JvFillIntf;
{$I JVCL.INC}
interface
uses
  Windows, ImgList, Classes, Graphics;

type
  TJvFillerChangeReason = (frAdd,     // an item is added
                           frDelete,  // an item is removed
                           frUpdate,  // the entire list is updated
                           frDestroy //  the IFiller implementor is being destroyed, clients should call UnRegisterChangeNotify and remove the reference to Filler
                           );

  TJvFillerSupport = (fsText,       // supports IFillerItemText
                      fsImages,     // supports IFillerItemImages
                      fsImageIndex, // supports IFillerItemImage
                      fsReadOnly,   // does *not* support IFillerItemManagment
                      fsCanRender,  // can render it's content to a DC
                      fsCanMeasure, // can measure the size of it's content
                      fsSubItems    // supports IFillerSubItems
                      );
  TJvFillerSupports = set of TJvFillerSupport;

  TJvFillerItemsAttribute = (fiaDynamicItems);
  TJvFillerItemsAttributes = set of TJvFillerItemsAttribute;

  // forward
  IFiller = interface;
  IFillerItems = interface;
  IFillerItem = interface;
  IFillerNotify = interface;

  TJvFillerOptions = class;
  TJvFillerOptionsClass = class of TJvFillerOptions;

  { base interface for components that supports storing lists of data (0..M items) }
  IFiller = interface
  ['{62A7A17D-1E21-427E-861D-C92FBB9B09A6}']
    procedure RegisterChangeNotify(AFillerNotify: IFillerNotify);
    procedure UnRegisterChangeNotify(AFillerNotify: IFillerNotify);
    function getSupports:TJvFillerSupports;
    function getOptionClass: TJvFillerOptionsClass;
    function getItems: IFillerItems;
    procedure NotifyConsumers(ChangeReason: TJvFillerChangeReason);
  end;

  { Item list. (0..N items)
    Implemented by IFiller implementors
    Supported by IFillerItem implementers only when fsSubItems is in IFiller.FillerSupports. }
  IFillerItems = interface
  ['{93747660-24FB-4294-BF4E-C7F88EA23983}']
    function getCount: integer;
    function getItem(Index: integer): IFillerItem;
    function GetParent: IFillerItem; // returns nil for the IFiller implementation
    function GetFiller: IFiller;
    function Attributes: TJvFillerItemsAttributes;
    function GetImplementer: TObject;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: IFillerItem read GetItem;
    property Parent: IFillerItem read GetParent;
    property Filler: IFiller read GetFiller;
  end;

  { Rendering interface. Provides support for both rendering and measuring of items.
    Implemented by IFillerItems. }
  IFillerItemsRenderer = interface
    ['{4EA490F4-7CCF-44A1-AA26-5320CDE9FAFC}']
    procedure DrawItemByIndex(ACanvas:TCanvas; var ARect: TRect; Index: Integer; State: TOwnerDrawState; AOptions: TPersistent = nil);
    function MeasureItemByIndex(ACanvas:TCanvas; Index: Integer; AOptions: TPersistent = nil): TSize;
    procedure DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem; State: TOwnerDrawState; AOptions: TPersistent = nil);
    function MeasureItem(ACanvas: TCanvas; Item: IFillerItem; AOptions: TPersistent = nil): TSize;
    function AvgItemSize(ACanvas: TCanvas; AOptions: TPersistent = nil): TSize;
  end;

  { Rendering interface for an item. Provides support for both rendering and measuring of the item.
    Implemneted by IFillerItem. }
  IFillerItemRenderer = interface
    ['{9E877A0D-01C2-4204-AA74-84D6516BBEB9}']
    procedure Draw(ACanvas: TCanvas; var ARect: TRect; State: TOwnerDrawState; AOptions: TPersistent = nil);
    function Measure(ACanvas:TCanvas; AOptions: TPersistent = nil): TSize;
  end;

  { base item interface: holds reference to the IFillerItems owner as well as provide a reference to the implementer }
  IFillerItem = interface
  ['{C965CF64-A1F2-44A4-B856-3A4EC6B693E1}']
    function GetItems: IFillerItems;
    function GetImplementer: TObject;
    function GetID: string;

    property Items: IFillerItems read GetItems;
    property Implementer: TObject read GetImplementer;
  end;

  { Implemented by clients (i.e list/comboboxes, labels, buttons, edits, listviews, treeviews, menus etc)
   to get notifications from IFiller }
  IFillerNotify = interface
  ['{5B9D1847-6D35-4D9C-8BC2-2054997AB120}']
    procedure FillerChanging(const AFiller: IFiller; AReason: TJvFillerChangeReason);
    procedure FillerChanged(const AFiller: IFiller; AReason: TJvFillerChangeReason);
  end;

  { base search interface. Can be supported by both IFiller and IFillerSubItems implementers if the
    implementation needs it.

    The basic idea is to declare additional interfaces to implement searching on other properties
    as well. eg. for the color filler:

      IFillerColorSearch = interface
        function IndexOfTColor(Color: TColor; const Recursive: Boolean = False): Integer;
      end;

    Both IFiller and IFillerSubItems implement those search interface that apply for the implementation.
    The recursive parameter has a default parameter value so it could be left out. }

  IFillerIDSearch = interface
    ['{0F5BDC79-893B-45C9-94E9-C2B2FD4ABFE7}']
    function FindByID(ID: string; const Recursive: Boolean = False): IFillerItem;
  end;

  IFillerTextSearch = interface
  ['{E3BC388D-50F6-402D-9E30-36D5F7F40616}']
    function FindByText(Text: string; const Recursive: Boolean = False): IFillerItem;
  end;

  { Implemented by servers that allows editing the list. Supported by IFillerItems implementers. }
  IFillerItemManagment = interface
  ['{76611CC0-9DCD-4394-8B6E-1ADEF1942BC3}']
    function Add(Item: IFillerItem): IFillerItem;
    function New: IFillerItem;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Remove(Item: IFillerItem);
  end;

  { Support interface for filler editor. May be implemented by IFillerItemManagment implementers who
    allow their list/tree to be edited. }
  IFillerItemsDesigner = interface
    ['{31B2544C-8E4F-40FE-94B8-04243EF40821}']
    function ItemKinds: string;
    function NewKind(Kind: Integer): IFillerItem;
  end;

  { only supported when fsImages is in  IFiller.FillerSupports; supported by IFillerItems implementers. }
  IFillerItemImages = interface
  ['{735755A6-AD11-460C-B985-46464D73EDBC}']
    function getImageList: TCustomImageList;
    procedure setImageList(const Value: TCustomImageList);
    property ImageList: TCustomImageList read getImageList write setImageList;
  end;

  { only supported when fsText is in IFiller.FillerSupports; supported by the IFillerItem implementer }
  IFillerItemText = interface
  ['{94FA56D9-281B-4252-B46D-15E7BADA70DA}']
    function getCaption: string;
    procedure setCaption(const Value: string);
    property Caption: string read getCaption write setCaption;
  end;

  { only supported when fsImageIndex is in IFiller.FillerSupports;
    supported by the IFillerItem implementer.
    Note that the IFiller probably need to implement the
    IFillerItemImages interface as well }
  IFillerItemImage = interface
  ['{6425D73A-90CF-42ED-9AB2-63125A4C0774}']
    function getAlignment: TAlignment;
    procedure setAlignment(Value: TAlignment);
    function getImageIndex: integer;
    procedure setImageIndex(Index: integer);
    function getSelectedIndex: integer;
    procedure setSelectedIndex(Value: integer);

    property Alignment: TAlignment read getAlignment write setAlignment;
    property ImageIndex: Integer read getImageIndex write setImageIndex;
    property SelectedIndex: Integer read getSelectedIndex write setSelectedIndex;
  end;

  { implemented by servers that supports a default action for each item }
  IFillerItemBasicAction = interface
  ['{86859A20-560D-4E9A-AC8B-2457789451B0}']
    function Execute(Index: Integer): Boolean;
  end;

  {$IFNDEF COMPILER6_UP}
  { Needed in D5 to use components with interface. Declaration copied from D6 Classes.pas. See
    various filler implementations for details. This declaration should probably be moved to
    JvTypes or JvComponents. }
  IInterfaceComponentReference = interface
    ['{E28B1858-EC86-4559-8FCD-6B4F824151ED}']
    function GetComponent: TComponent;
  end;
  {$ENDIF}

  { base class for options that are dynamically added to the client by the server implementation,
    see JvFillBasicImpl for details }
  TJvFillerOptions = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
  protected
    procedure Changed;
  public
    constructor Create(AOnChanged: TNotifyEvent); virtual;
  end;

implementation

{ TJvFillerOptions }

procedure TJvFillerOptions.Changed;
begin
  if @FOnChanged <> nil then
    FOnChanged(Self);
end;

constructor TJvFillerOptions.Create(AOnChanged: TNotifyEvent);
begin
  inherited Create;
  FOnChanged := AOnChanged;
end;

end.



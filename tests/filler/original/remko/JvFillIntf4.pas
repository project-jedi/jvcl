unit JvFillIntf4;

{$I JVCL.INC}
interface
uses
{$IFDEF COMPILER6_UP}Types, Controls{$ELSE}Windows, ImgList{$ENDIF}, Classes;

type
  TJvFillerChangeReason = (
    frAdd, // an item is added
    frDelete, // an item is removed
    frUpdate, // the entire list is updated
    frDestroy //  the IFiller implementor is being destroyed, clients should call UnRegisterChangeNotify and remove the reference to Filler
    );

  TJvFillerSupport = (
    fsText, // supports IFillerItemText
    fsImages, // supports IFillerItemImages
    fsImageIndex, // supports IFillerItemImage
    fsReadOnly, // does *not* support IFillerItemManagment
    fsCanRender, // can render it's content to a DC
    fsCanMeasure, // can measure the size of it's content
    fsSubItems // supports IFillerSubItems
    );

  TJvFillerSupports = set of TJvFillerSupport;

  // forward
  IFillerNotify = interface;

  { base item interface: supports nothing }
  IFillerItem = interface
    ['{93CC692E-6289-4107-AEF4-E2B94BFDBE29}']
  end;

  { base interface for components that supports storing lists of data (0..M items) }
  IBaseFiller = interface
    ['{515ADAD6-726A-438C-9AFD-D8E9391B6E12}']
    procedure RegisterChangeNotify(AFillerNotify: IFillerNotify);
    procedure UnRegisterChangeNotify(AFillerNotify: IFillerNotify);
  end;

  IFiller = interface(IBaseFiller)
    ['{325B250D-C2B7-468B-B6C7-EF7289BDCD3D}']
    procedure DrawItem(hDC: LongWord; var ARect: TRect; Index: Integer);
    function MeasureItem(Index: Integer): TSize;
    function GetCount: Integer;
    function getItem(Index: Integer): IFillerItem;
    function getSupports: TJvFillerSupports;

    property Items[Index: Integer]: IFillerItem read getItem; default;
    property Count: Integer read GetCount;
  end;

  { Implemented by consumers (i.e list/comboboxes, labels, buttons, edits, listviews, treeviews, menus etc)
   to get notifications from IFiller }
  IFillerNotify = interface
    ['{3E479078-93CE-4A28-AD22-5BCF2D5F01BD}']
    procedure FillerChanging(const AFiller: IFiller; AReason:
      TJvFillerChangeReason);
    procedure FillerChanged(const AFiller: IFiller; AReason:
      TJvFillerChangeReason);
  end;

  { base search interface. Supported by both IFiller and IFillerSubItems implementers if the
    implementation needs it.

    The basic idea is to declare additional interfaces to implement searching on other properties
    as well. eg. for the color filler:

      IFillerColorSearch = interface
        function IndexOfTColor(Color: TColor; const Recursive: Boolean = False): Integer;
      end;

    Both IFiller and IFillerSubItems implement those search interface that apply for the implementation.
    The recursive parameter has a default parameter value so it could be left out. }

  IFillerTextSearch = interface
    function IndexOfText(Text: string; const Recursive: Boolean = False):
      Integer;
  end;

  { only supported for none autofillers. Supported by IFiller and IFillerSubItems implementers. }
  IFillerItemManagment = interface
    function Add: IFillerItem;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Remove(Item: IFiller);
  end;

  { only supported when fsImages is in  IFiller.FillerSupports; supported by either IFiller or IFillerSubItems implementers. }
  IFillerItemImages = interface
  ['{BE2C07C4-1B19-4AC4-B444-46D0025D8939}']
    function getImageList: TImageList;
    procedure setImageList(const Value: TImageList);
    property ImageList: TImageList read getImageList write setImageList;
  end;

  { only supported when fsText is in IFiller.FillerSupports; supported by the IFillerItem implementer }
  IFillerItemText = interface
    ['{C6BABC37-E71C-49B8-911A-C75BFDBFA2EE}']
    function getCaption: string;
    procedure setCaption(const Value: string);
    property Caption: string read getCaption write setCaption;
  end;

  { only supported when fsImageIndex is in IFiller.FillerSupports; supported by the IFillerItem implementer. }
  IFillerItemImage = interface
  ['{578F153B-B881-488F-B3E5-F6B7711E9EBA}']
    function getAlignment: TAlignment;
    procedure setAlignment(Value: TAlignment);
    function getImageIndex: Integer;
    procedure setImageIndex(Index: Integer);
    function getSelectedIndex: Integer;
    procedure setSelectedIndex(Value: Integer);

    property Alignment: TAlignment read getAlignment write setAlignment;
    property ImageIndex: Integer read getImageIndex write setImageIndex;
    property SelectedIndex: Integer read getSelectedIndex write
      setSelectedIndex;
  end;

  { only supported when fsSubItems is in IFiller.FillerSupports. Supported by the IFillerItem implementer. }
  IFillerSubItems = interface
    function GetCount: Integer;
    function getItem(i: Integer): IFillerItem;
    function GetParent: IFillerItem;
    property Items[Index: Integer]: IFillerItem read getItem; default;
  end;

implementation

end.


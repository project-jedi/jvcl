
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{       Copyright (c) 1995,99 Inprise Corporation       }
{                                                       }
{*******************************************************}

unit DsgnIntf;

interface

{$N+,S-,R-}

uses
  Windows, Activex, SysUtils, Classes, Graphics, Controls, Forms, Contnrs, IniFiles,
  TypInfo, Masks, Menus;

type

  TEditAction = (eaUndo, eaRedo, eaCut, eaCopy, eaPaste, eaDelete, eaSelectAll,
    eaPrint, eaBringToFront, eaSendToBack, eaAlignToGrid, eaFlipChildrenAll,
    eaFlipChildrenSelected);

  TEditState = set of (esCanUndo, esCanRedo, esCanCut, esCanCopy, esCanPaste,
    esCanDelete, esCanZOrder, esCanAlignGrid, esCanEditOle, esCanTabOrder,
	esCanCreationOrder, esCanPrint, esCanSelectAll);

  IEventInfos = interface
    ['{11667FF0-7590-11D1-9FBC-0020AF3D82DA}']
    function GetCount: Integer;
    function GetEventValue(Index: Integer): string;
    function GetEventName(Index: Integer): string;
    procedure ClearEvent(Index: Integer);
    property Count: Integer read GetCount;
  end;

  IPersistent = interface
    ['{82330133-65D1-11D1-9FBB-0020AF3D82DA}'] {Java}
    procedure DestroyObject;
    function Equals(const Other: IPersistent): Boolean;
    function GetClassname: string;
    function GetEventInfos: IEventInfos;
    function GetNamePath: string;
    function GetOwner: IPersistent;
    function InheritsFrom(const Classname: string): Boolean;
    function IsComponent: Boolean;  // object is stream createable
    function IsControl: Boolean;
    function IsWinControl: Boolean;
    property Classname: string read GetClassname;
    property Owner: IPersistent read GetOwner;
    property NamePath: string read GetNamePath;
//    property PersistentProps[Index: Integer]: IPersistent
//    property PersistentPropCount: Integer;
    property EventInfos: IEventInfos read GetEventInfos;
  end;

  IComponent = interface(IPersistent)
    ['{B2F6D681-5098-11D1-9FB5-0020AF3D82DA}'] {Java}
    function FindComponent(const Name: string): IComponent;
    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): IComponent;
    function GetComponentState: TComponentState;
    function GetComponentStyle: TComponentStyle;
    function GetDesignInfo: TSmallPoint;
    function GetDesignOffset: TPoint;
    function GetDesignSize: TPoint;
    function GetName: string;
    function GetOwner: IComponent;
    function GetParent: IComponent;
    procedure SetDesignInfo(const Point: TSmallPoint);
    procedure SetDesignOffset(const Point: TPoint);
    procedure SetDesignSize(const Point: TPoint);
    procedure SetName(const Value: string);
    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: IComponent read GetComponents;
    property ComponentState: TComponentState read GetComponentState;
    property ComponentStyle: TComponentStyle read GetComponentStyle;
    property DesignInfo: TSmallPoint read GetDesignInfo write SetDesignInfo;
    property DesignOffset: TPoint read GetDesignOffset write SetDesignOffset;
    property DesignSize: TPoint read GetDesignSize write SetDesignSize;
    property Name: string read GetName write SetName;
    property Owner: IComponent read GetOwner;
    property Parent: IComponent read GetParent;
  end;

  IImplementation = interface
    ['{F9D448F2-50BC-11D1-9FB5-0020AF3D82DA}']
    function GetInstance: TObject;
  end;

  function MakeIPersistent(Instance: TPersistent): IPersistent;
  function ExtractPersistent(const Intf: IUnknown): TPersistent;
  function TryExtractPersistent(const Intf: IUnknown): TPersistent;

  function MakeIComponent(Instance: TComponent): IComponent;
  function ExtractComponent(const Intf: IUnknown): TComponent;
  function TryExtractComponent(const Intf: IUnknown): TComponent;

var
  MakeIPersistentProc: function (Instance: TPersistent): IPersistent = nil;
  MakeIComponentProc: function (Instance: TComponent): IComponent = nil;

type

{ IDesignerSelections  }
{   Used to transport the selected objects list in and out of the form designer.
    Replaces TDesignerSelectionList in form designer interface.  }

  IDesignerSelections = interface
    ['{82330134-65D1-11D1-9FBB-0020AF3D82DA}'] {Java}
    function Add(const Item: IPersistent): Integer;
    function Equals(const List: IDesignerSelections): Boolean;
    function Get(Index: Integer): IPersistent;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IPersistent read Get; default;
  end;

function CreateSelectionList: IDesignerSelections;

type

  TDesignerSelectionList = class;

  IComponentList = interface
    ['{8ED8AD16-A241-11D1-AA94-00C04FB17A72}']
    function GetComponentList: TDesignerSelectionList;
  end;

{ TDesignerSelectionList }
{   Used to transport VCL component selections between property editors }

  TDesignerSelectionList = class(TInterfacedObject, IDesignerSelections,
    IComponentList)
  private
    FList: TList;
    { IDesignSelections }
    function IDesignerSelections.Add = Intf_Add;
    function Intf_Add(const Item: IPersistent): Integer;
    function IDesignerSelections.Equals = Intf_Equals;
    function Intf_Equals(const List: IDesignerSelections): Boolean;
    function IDesignerSelections.Get = Intf_Get;
    function Intf_Get(Index: Integer): IPersistent;
    function Get(Index: Integer): TPersistent;
    function GetCount: Integer;
    { IComponentList }
    function GetComponentList: TDesignerSelectionList;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: TPersistent): Integer;
    function Equals(List: TDesignerSelectionList): Boolean;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TPersistent read Get; default;
  end;

{ IFormDesigner
    BuildLocalMenu - Constructs and returns the popup menu for the currently
    selected component(s).  Base is the popup menu that will receive additional
    menu items.  If Base is nil, a default popup menu is constructed containing
    the default designer menu items, like "Align to Grid".  The menu object
    returned by this function is owned by the designer and will be destroyed
    the next time BuildLocalMenu is called (the next time a Popup menu is
    invoked on the designer).  If you pass in a Base menu, you don't own it
    anymore.  It will be destroyed later.
}
type
  TLocalMenuFilter = (lmModule, lmComponent, lmDesigner);
  TLocalMenuFilters = set of TLocalMenuFilter;

const
  cNoLocalMenus = [lmModule, lmComponent, lmDesigner];
  cAllLocalMenus = [];
  cLocalMenusIf: array [boolean] of TLocalMenuFilters =
    (cNoLocalMenus, cAllLocalMenus);

type
  IFormDesigner = interface(IDesigner)
    ['{ADDD444D-1B03-11D3-A8F8-00C04FA32F53}']
    function CreateMethod(const Name: string; TypeData: PTypeData): TMethod;
    function GetMethodName(const Method: TMethod): string;
    procedure GetMethods(TypeData: PTypeData; Proc: TGetStrProc);
    function GetPrivateDirectory: string;
    procedure GetSelections(const List: IDesignerSelections);
    function MethodExists(const Name: string): Boolean;
    procedure RenameMethod(const CurName, NewName: string);
    procedure SelectComponent(Instance: TPersistent);
    procedure SetSelections(const List: IDesignerSelections);
    procedure ShowMethod(const Name: string);
    procedure GetComponentNames(TypeData: PTypeData; Proc: TGetStrProc);
    function GetComponent(const Name: string): TComponent;
    function GetComponentName(Component: TComponent): string;
    function GetObject(const Name: string): TPersistent;
    function GetObjectName(Instance: TPersistent): string;
    procedure GetObjectNames(TypeData: PTypeData; Proc: TGetStrProc);
    function MethodFromAncestor(const Method: TMethod): Boolean;
    function CreateComponent(ComponentClass: TComponentClass; Parent: TComponent;
      Left, Top, Width, Height: Integer): TComponent;
    function IsComponentLinkable(Component: TComponent): Boolean;
    procedure MakeComponentLinkable(Component: TComponent);
    procedure Revert(Instance: TPersistent; PropInfo: PPropInfo);
    function GetIsDormant: Boolean;
    function HasInterface: Boolean;
    function HasInterfaceMember(const Name: string): Boolean;
    procedure AddToInterface(InvKind: Integer; const Name: string; VT: Word;
      const TypeInfo: string);
    procedure GetProjectModules(Proc: TGetModuleProc);
    function GetAncestorDesigner: IFormDesigner;
    function IsSourceReadOnly: Boolean;
    function GetContainerWindow: TWinControl;
    procedure SetContainerWindow(const NewContainer: TWinControl);
    function GetScrollRanges(const ScrollPosition: TPoint): TPoint;
    procedure Edit(const Component: IComponent);
    function BuildLocalMenu(Base: TPopupMenu; Filter: TLocalMenuFilters): TPopupMenu;
    procedure ChainCall(const MethodName, InstanceName, InstanceMethod: string;
      TypeData: PTypeData);
    procedure CopySelection;
    procedure CutSelection;
    function CanPaste: Boolean;
    procedure PasteSelection;
    procedure DeleteSelection;
    procedure ClearSelection;
    procedure NoSelection;
    procedure ModuleFileNames(var ImplFileName, IntfFileName, FormFileName: string);
    function GetRootClassName: string;
    property IsDormant: Boolean read GetIsDormant;
    property AncestorDesigner: IFormDesigner read GetAncestorDesigner;
    property ContainerWindow: TWinControl read GetContainerWindow write SetContainerWindow;
  end;

  IDesignNotification = interface
    ['{3250122F-D336-11D2-B725-00C04FA35D12}']
    procedure ItemDeleted(const AItem: IPersistent);
    procedure ItemInserted(const AItem: IPersistent);
    procedure ItemsModified(const ADesigner: IUnknown);
    procedure SelectionChanged(const ASelection: IDesignerSelections);
    procedure DesignerInitialized(const ADesigner: IUnknown);
    procedure DesignerClosed(const ADesigner: IUnknown);
  end;

{ IDesignerPopulateMenu
    Allows a design surface an opportunity to add context-sensitive menu items
    to the designer's popup menu.  This is in addition to the component editor
    verbs and the custom module verbs. }

  IDesignerPopulateMenu = interface
    ['{66C7D913-EC70-11D2-AAD1-00C04FB16FBC}']
    procedure PopulateMenu(const APopupMenu: TPopupMenu);
  end;

{ TPropertyEditor
  Edits a property of a component, or list of components, selected into the
  Object Inspector.  The property editor is created based on the type of the
  property being edited as determined by the types registered by
  RegisterPropertyEditor.  The Object Inspector uses a TPropertyEditor
  for all modification to a property. GetName and GetValue are called to display
  the name and value of the property.  SetValue is called whenever the user
  requests to change the value.  Edit is called when the user double-clicks the
  property in the Object Inspector. GetValues is called when the drop-down
  list of a property is displayed.  GetProperties is called when the property
  is expanded to show sub-properties.  AllEqual is called to decide whether or
  not to display the value of the property when more than one component is
  selected.

  The following are methods that can be overridden to change the behavior of
  the property editor:

    Activate
      Called whenever the property becomes selected in the object inspector.
      This is potentially useful to allow certain property attributes to
      to only be determined whenever the property is selected in the object
      inspector. Only paSubProperties and paMultiSelect, returned from
      GetAttributes, need to be accurate before this method is called.
    AllEqual
      Called whenever there is more than one component selected.  If this
      method returns true, GetValue is called, otherwise blank is displayed
      in the Object Inspector.  This is called only when GetAttributes
      returns paMultiSelect.
    AutoFill
      Called to determine whether the values returned by GetValues can be
      selected incrementally in the Object Inspector.  This is called only when
      GetAttributes returns paValueList.
    Edit
      Called when the '...' button is pressed or the property is double-clicked.
      This can, for example, bring up a dialog to allow the editing the
      component in some more meaningful fashion than by text (e.g. the Font
      property).
    GetAttributes
      Returns the information for use in the Object Inspector to be able to
      show the appropriate tools.  GetAttributes returns a set of type
      TPropertyAttributes:
        paValueList:     The property editor can return an enumerated list of
                         values for the property.  If GetValues calls Proc
                         with values then this attribute should be set.  This
                         will cause the drop-down button to appear to the right
                         of the property in the Object Inspector.
        paSortList:      Object Inspector to sort the list returned by
                         GetValues.
        paSubProperties: The property editor has sub-properties that will be
                         displayed indented and below the current property in
                         standard outline format. If GetProperties will
                         generate property objects then this attribute should
                         be set.
        paDialog:        Indicates that the Edit method will bring up a
                         dialog.  This will cause the '...' button to be
                         displayed to the right of the property in the Object
                         Inspector.
        paMultiSelect:   Allows the property to be displayed when more than
                         one component is selected.  Some properties are not
                         appropriate for multi-selection (e.g. the Name
                         property).
        paAutoUpdate:    Causes the SetValue method to be called on each
                         change made to the editor instead of after the change
                         has been approved (e.g. the Caption property).
        paReadOnly:      Value is not allowed to change.
        paRevertable:    Allows the property to be reverted to the original
                         value.  Things that shouldn't be reverted are nested
                         properties (e.g. Fonts) and elements of a composite
                         property such as set element values.
        paFullWidthName: Tells the object inspector that the value does not
                         need to be rendered and as such the name should be
                         rendered the full width of the inspector.
    GetComponent
      Returns the Index'th component being edited by this property editor.  This
      is used to retrieve the components.  A property editor can only refer to
      multiple components when paMultiSelect is returned from GetAttributes.
    GetEditLimit
      Returns the number of character the user is allowed to enter for the
      value.  The inplace editor of the object inspector will be have its
      text limited set to the return value.  By default this limit is 255.
    GetName
      Returns the name of the property.  By default the value is retrieved
      from the type information with all underbars replaced by spaces.  This
      should only be overridden if the name of the property is not the name
      that should appear in the Object Inspector.
    GetProperties
      Should be overridden to call PropertyProc for every sub-property (or
      nested property) of the property begin edited and passing a new
      TPropertyEdtior for each sub-property.  By default, PropertyProc is not
      called and no sub-properties are assumed.  TClassProperty will pass a
      new property editor for each published property in a class.  TSetProperty
      passes a new editor for each element in the set.
    GetPropType
      Returns the type information pointer for the property(s) being edited.
    GetValue
      Returns the string value of the property. By default this returns
      '(unknown)'.  This should be overridden to return the appropriate value.
    GetValues
      Called when paValueList is returned in GetAttributes.  Should call Proc
      for every value that is acceptable for this property.  TEnumProperty
      will pass every element in the enumeration.
    Initialize
      Called after the property editor has been created but before it is used.
      Many times property editors are created and because they are not a common
      property across the entire selection they are thrown away.  Initialize is
      called after it is determined the property editor is going to be used by
      the object inspector and not just thrown away.
    SetValue(Value)
      Called to set the value of the property.  The property editor should be
      able to translate the string and call one of the SetXxxValue methods. If
      the string is not in the correct format or not an allowed value, the
      property editor should generate an exception describing the problem. Set
      value can ignore all changes and allow all editing of the property be
      accomplished through the Edit method (e.g. the Picture property).
    ListMeasureWidth(Value, Canvas, AWidth)
      This is called during the width calculation phase of the drop down list
      preparation.
    ListMeasureHeight(Value, Canvas, AHeight)
      This is called during the item/value height calculation phase of the drop
      down list's render.  This is very similar to TListBox's OnMeasureItem,
      just slightly different parameters.
    ListDrawValue(Value, Canvas, Rect, Selected)
      This is called during the item/value render phase of the drop down list's
      render.  This is very similar to TListBox's OnDrawItem, just slightly
      different parameters.
    PropDrawName(Canvas, Rect, Selected)
      Called during the render of the name column of the property list.  Its
      functionality is very similar to TListBox's OnDrawItem, but once again
      it has slightly different parameters.
    PropDrawValue(Canvas, Rect, Selected)
      Called during the render of the value column of the property list.  Its
      functionality is similar to PropDrawName.  If multiple items are selected
      and their values don't match this procedure will be passed an empty
      value.

  Properties and methods useful in creating a new TPropertyEditor classes:

    Name property
      Returns the name of the property returned by GetName
    PrivateDirectory property
      It is either the .EXE or the "working directory" as specified in
      the registry under the key:
        "HKEY_CURRENT_USER\Software\Borland\Delphi\*\Globals\PrivateDir"
      If the property editor needs auxiliary or state files (templates, examples,
      etc) they should be stored in this directory.
    Value property
      The current value, as a string, of the property as returned by GetValue.
    Modified
      Called to indicate the value of the property has been modified.  Called
      automatically by the SetXxxValue methods.  If you call a TProperty
      SetXxxValue method directly, you *must* call Modified as well.
    GetXxxValue
      Gets the value of the first property in the Properties property.  Calls
      the appropriate TProperty GetXxxValue method to retrieve the value.
    SetXxxValue
      Sets the value of all the properties in the Properties property.  Calls
      the approprate TProperty SetXxxxValue methods to set the value.
    GetVisualValue
      This function will return the displayable value of the property.  If
      only one item is selected or all the multi-selected items have the same
      property value then this function will return the actual property value.
      Otherwise this function will return an empty string.}

  TPropertyAttribute = (paValueList, paSubProperties, paDialog, paMultiSelect,
    paAutoUpdate, paSortList, paReadOnly, paRevertable, paFullWidthName);
  TPropertyAttributes = set of TPropertyAttribute;

  TPropertyEditor = class;

  TInstProp = record
    Instance: TPersistent;
    PropInfo: PPropInfo;
  end;

  PInstPropList = ^TInstPropList;
  TInstPropList = array[0..1023] of TInstProp;

  TGetPropEditProc = procedure(Prop: TPropertyEditor) of object;

  TPropertyEditor = class
  private
    FDesigner: IFormDesigner;
    FPropList: PInstPropList;
    FPropCount: Integer;
    function GetPrivateDirectory: string;
    procedure SetPropEntry(Index: Integer; AInstance: TPersistent;
      APropInfo: PPropInfo);
  protected
    constructor Create(const ADesigner: IFormDesigner; APropCount: Integer); virtual;
    function GetPropInfo: PPropInfo;
    function GetFloatValue: Extended;
    function GetFloatValueAt(Index: Integer): Extended;
    function GetInt64Value: Int64;
    function GetInt64ValueAt(Index: Integer): Int64;
    function GetMethodValue: TMethod;
    function GetMethodValueAt(Index: Integer): TMethod;
    function GetOrdValue: Longint;
    function GetOrdValueAt(Index: Integer): Longint;
    function GetStrValue: string;
    function GetStrValueAt(Index: Integer): string;
    function GetVarValue: Variant;
    function GetVarValueAt(Index: Integer): Variant;
    procedure Modified; 
    procedure SetFloatValue(Value: Extended);
    procedure SetMethodValue(const Value: TMethod);
    procedure SetInt64Value(Value: Int64);
    procedure SetOrdValue(Value: Longint);
    procedure SetStrValue(const Value: string);
    procedure SetVarValue(const Value: Variant);
  public
    destructor Destroy; override;
    procedure Activate; virtual;
    function AllEqual: Boolean; virtual;
    function AutoFill: Boolean; virtual;
    procedure Edit; virtual;
    function GetAttributes: TPropertyAttributes; virtual;
    function GetComponent(Index: Integer): TPersistent;
    function GetEditLimit: Integer; virtual;
    function GetName: string; virtual;
    procedure GetProperties(Proc: TGetPropEditProc); virtual;
    function GetPropType: PTypeInfo;
    function GetValue: string; virtual;
    function GetVisualValue: string;
    procedure GetValues(Proc: TGetStrProc); virtual;
    procedure Initialize; virtual;
    procedure Revert;
    procedure SetValue(const Value: string); virtual;
    function ValueAvailable: Boolean;
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer); dynamic;
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer); dynamic;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); dynamic;
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean); dynamic;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean); dynamic;
    property Designer: IFormDesigner read FDesigner;
    property PrivateDirectory: string read GetPrivateDirectory;
    property PropCount: Integer read FPropCount;
    property Value: string read GetValue write SetValue;
  end;

  TPropertyEditorClass = class of TPropertyEditor;

{ TOrdinalProperty
  The base class of all ordinal property editors.  It established that ordinal
  properties are all equal if the GetOrdValue all return the same value. }

  TOrdinalProperty = class(TPropertyEditor)
    function AllEqual: Boolean; override;
    function GetEditLimit: Integer; override;
  end;

{ TIntegerProperty
  Default editor for all Longint properties and all subtypes of the Longint
  type (i.e. Integer, Word, 1..10, etc.).  Restricts the value entered into
  the property to the range of the sub-type. }

  TIntegerProperty = class(TOrdinalProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TCharProperty
  Default editor for all Char properties and sub-types of Char (i.e. Char,
  'A'..'Z', etc.). }

  TCharProperty = class(TOrdinalProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TEnumProperty
  The default property editor for all enumerated properties (e.g. TShape =
  (sCircle, sTriangle, sSquare), etc.). }

  TEnumProperty = class(TOrdinalProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TBoolProperty = class(TEnumProperty)
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{ TInt64Property
  Default editor for all Int64 properties and all subtypes of Int64.  }

  TInt64Property = class(TPropertyEditor)
  public
    function AllEqual: Boolean; override;
    function GetEditLimit: Integer; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TFloatProperty
  The default property editor for all floating point types (e.g. Float,
  Single, Double, etc.) }

  TFloatProperty = class(TPropertyEditor)
  public
    function AllEqual: Boolean; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TStringProperty
  The default property editor for all strings and sub types (e.g. string,
  string[20], etc.). }

  TStringProperty = class(TPropertyEditor)
  public
    function AllEqual: Boolean; override;
    function GetEditLimit: Integer; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TNestedProperty
  A property editor that uses the parent's Designer, PropList and PropCount.
  The constructor and destructor do not call inherited, but all derived classes
  should.  This is useful for properties like the TSetElementProperty. }

  TNestedProperty = class(TPropertyEditor)
  public
    constructor Create(Parent: TPropertyEditor); reintroduce;
    destructor Destroy; override;
  end;

{ TSetElementProperty
  A property editor that edits an individual set element.  GetName is
  changed to display the set element name instead of the property name and
  Get/SetValue is changed to reflect the individual element state.  This
  editor is created by the TSetProperty editor. }

  TSetElementProperty = class(TNestedProperty)
  private
    FElement: Integer;
  protected
    constructor Create(Parent: TPropertyEditor; AElement: Integer); reintroduce;
  public
    function AllEqual: Boolean; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetName: string; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
   end;

{ TSetProperty
  Default property editor for all set properties. This editor does not edit
  the set directly but will display sub-properties for each element of the
  set. GetValue displays the value of the set in standard set syntax. }

  TSetProperty = class(TOrdinalProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function GetValue: string; override;
  end;

{ TClassProperty
  Default property editor for all objects.  Does not allow modifying the
  property but does display the class name of the object and will allow the
  editing of the object's properties as sub-properties of the property. }

  TClassProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function GetValue: string; override;
  end;

{ TMethodProperty
  Property editor for all method properties. }

  TMethodProperty = class(TPropertyEditor)
  public
    function AllEqual: Boolean; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const AValue: string); override;
    function GetFormMethodName: string; virtual;
    function GetTrimmedEventName: string;
  end;

{ TComponentProperty
  The default editor for TComponents.  It does not allow editing of the
  properties of the component.  It allow the user to set the value of this
  property to point to a component in the same form that is type compatible
  with the property being edited (e.g. the ActiveControl property). }

  TComponentProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{ TComponentNameProperty
  Property editor for the Name property.  It restricts the name property
  from being displayed when more than one component is selected. }

  TComponentNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
  end;

{ TFontNameProperty
  Editor for the TFont.FontName property.  Displays a drop-down list of all
  the fonts known by Windows.  The following global variable will make
  this property editor actually show examples of each of the fonts in the
  drop down list.  We would have enabled this by default but it takes
  too many cycles on slower machines or those with a lot of fonts.  Enable
  it at your own risk. ;-}
var
  FontNamePropertyDisplayFontNames: Boolean = False;

type
  TFontNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;

    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer); override;
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer); override;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); override;
  end;

{ TFontCharsetProperty
  Editor for the TFont.Charset property.  Displays a drop-down list of the
  character-set by Windows.}

  TFontCharsetProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{ TImeNameProperty
  Editor for the TImeName property.  Displays a drop-down list of all
  the IME names known by Windows.}

  TImeNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TColorProperty
  Property editor for the TColor type.  Displays the color as a clXXX value
  if one exists, otherwise displays the value as hex.  Also allows the
  clXXX value to be picked from a list. }

  TColorProperty = class(TIntegerProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;

    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer); override;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean); override;
  end;

{ TBrushStyleProperty
  Property editor for TBrush's Style.  Simply provides for custom render. }

  TBrushStyleProperty = class(TEnumProperty)
  public
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer); override;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean); override;
  end;

{ TPenStyleProperty
  Property editor for TPen's Style.  Simply provides for custom render. }

  TPenStyleProperty = class(TEnumProperty)
  public
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer); override;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean); override;
  end;

{ TCursorProperty
  Property editor for the TCursor type.  Displays the cursor as a clXXX value
  if one exists, otherwise displays the value as hex.  Also allows the
  clXXX value to be picked from a list. }

  TCursorProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer); override;
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer); override;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); override;
  end;

{ TFontProperty
  Property editor for the Font property.  Brings up the font dialog as well as
  allowing the properties of the object to be edited. }

  TFontProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TModalResultProperty }

  TModalResultProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{ TShortCutProperty
  Property editor the ShortCut property.  Allows both typing in a short
  cut value or picking a short-cut value from a list. }

  TShortCutProperty = class(TOrdinalProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{ TMPFilenameProperty
  Property editor for the TMediaPlayer.  Displays an File Open Dialog
  for the name of the media file.}

  TMPFilenameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TTabOrderProperty
  Property editor for the TabOrder property.  Prevents the property from being
  displayed when more than one component is selected. }

  TTabOrderProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TCaptionProperty
  Property editor for the Caption and Text properties.  Updates the value of
  the property for each change instead on when the property is approved. }

  TCaptionProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TDateProperty
  Property editor for date portion of TDateTime type. }

  TDateProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TTimeProperty
  Property editor for time portion of TDateTime type. }

  TTimeProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TDateTimeProperty
  Edits both date and time data... simultaneously!  }

  TDateTimeProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TComponentEditor
  A component editor is created for each component that is selected in the
  form designer based on the component's type (see GetComponentEditor and
  RegisterComponentEditor).  When the component is double-clicked the Edit
  method is called.  When the context menu for the component is invoked the
  GetVerbCount and GetVerb methods are called to build the menu.  If one
  of the verbs are selected ExecuteVerb is called.  Paste is called whenever
  the component is pasted to the clipboard.  You only need to create a
  component editor if you wish to add verbs to the context menu, change
  the default double-click behavior, or paste an additional clipboard format.
  The default component editor (TDefaultEditor) implements Edit to searches the
  properties of the component and generates (or navigates to) the OnCreate,
  OnChanged, or OnClick event (whichever it finds first).  Whenever the
  component modifies the component is *must* call Designer.Modified to inform
  the designer that the form has been modified.

    Create(AComponent, ADesigner)
      Called to create the component editor.  AComponent is the component to
      be edited by the editor.  ADesigner is an interface to the designer to
      find controls and create methods (this is not use often).
    Edit
      Called when the user double-clicks the component. The component editor can
      bring up a dialog in response to this method, for example, or some kind
      of design expert.  If GetVerbCount is greater than zero, edit will execute
      the first verb in the list (ExecuteVerb(0)).
    ExecuteVerb(Index)
      The Index'ed verb was selected by the use off the context menu.  The
      meaning of this is determined by component editor.
    GetVerb
      The component editor should return a string that will be displayed in the
      context menu.  It is the responsibility of the component editor to place
      the & character and the '...' characters as appropriate.
    GetVerbCount
      The number of valid indices to GetVerb and Execute verb.  The index is assumed
      to be zero based (i.e. 0..GetVerbCount - 1).
    PrepareItem
      While constructing the context menu PrepareItem will be called for
      each verb.  It will be passed the menu item that will be used to represent
      the verb.  The component editor can customize the menu item as it sees fit,
      including adding subitems.  If you don't want that particular menu item
      to be shown, don't free it, simply set its Visible property to False.
    Copy
      Called when the component is being copied to the clipboard.  The
      component's filed image is already on the clipboard.  This gives the
      component editor a chance to paste a different type of format which is
      ignored by the designer but might be recognized by another application.
    IsInInlined
      Determines whether Component is in the Designer which owns it.  Essentially,
      Components should not be able to be added to a Frame instance (collections
      are fine though) so this function checks to determine whether the currently
      selected component is within a Frame instance or not.
    }

  IComponentEditor = interface
    ['{ABBE7252-5495-11D1-9FB5-0020AF3D82DA}']
    procedure Edit;
    procedure ExecuteVerb(Index: Integer);
    function GetIComponent: IComponent;
    function GetDesigner: IFormDesigner;
    function GetVerb(Index: Integer): string;
    function GetVerbCount: Integer;
    procedure PrepareItem(Index: Integer; const AItem: TMenuItem);
    procedure Copy;
  end;

  TComponentEditor = class(TInterfacedObject, IComponentEditor)
  private
    FComponent: TComponent;
    FDesigner: IFormDesigner;
  public
    constructor Create(AComponent: TComponent; ADesigner: IFormDesigner); virtual;
    procedure Edit; virtual;
    procedure ExecuteVerb(Index: Integer); virtual;
    function GetIComponent: IComponent;
    function GetDesigner: IFormDesigner;
    function GetVerb(Index: Integer): string; virtual;
    function GetVerbCount: Integer; virtual;
    function IsInInlined: Boolean;
    procedure PrepareItem(Index: Integer; const AItem: TMenuItem); virtual;
    procedure Copy; virtual;
    property Component: TComponent read FComponent;
    property Designer: IFormDesigner read GetDesigner;
  end;

  TComponentEditorClass = class of TComponentEditor;

  IDefaultEditor = interface(IComponentEditor)
    ['{5484FAE1-5C60-11D1-9FB6-0020AF3D82DA}']
  end;

  TDefaultEditor = class(TComponentEditor, IDefaultEditor)
  private
    FFirst: TPropertyEditor;
    FBest: TPropertyEditor;
    FContinue: Boolean;
    procedure CheckEdit(PropertyEditor: TPropertyEditor);
  protected
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); virtual;
  public
    procedure Edit; override;
  end;

{ Global variables initialized internally by the form designer }

type
  TFreeCustomModulesProc = procedure (Group: Integer);

var
  FreeCustomModulesProc: TFreeCustomModulesProc;

{ RegisterPropertyEditor
  Registers a new property editor for the given type.  When a component is
  selected the Object Inspector will create a property editor for each
  of the component's properties.  The property editor is created based on
  the type of the property.  If, for example, the property type is an
  Integer, the property editor for Integer will be created (by default
  that would be TIntegerProperty). Most properties do not need specialized
  property editors.  For example, if the property is an ordinal type the
  default property editor will restrict the range to the ordinal subtype
  range (e.g. a property of type TMyRange = 1..10 will only allow values
  between 1 and 10 to be entered into the property).  Enumerated types will
  display a drop-down list of all the enumerated values (e.g. TShapes =
  (sCircle, sSquare, sTriangle) will be edited by a drop-down list containing
  only sCircle, sSquare and sTriangle).  A property editor need only be
  created if default property editor or none of the existing property editors
  are sufficient to edit the property.  This is typically because the
  property is an object.  The properties are looked up newest to oldest.
  This allows and existing property editor replaced by a custom property
  editor.

    PropertyType
      The type information pointer returned by the TypeInfo built-in function
      (e.g. TypeInfo(TMyRange) or TypeInfo(TShapes)).

    ComponentClass
      Type of the component to which to restrict this type editor.  This
      parameter can be left nil which will mean this type editor applies to all
      properties of PropertyType.

    PropertyName
      The name of the property to which to restrict this type editor.  This
      parameter is ignored if ComponentClass is nil.  This parameter can be
      an empty string ('') which will mean that this editor applies to all
      properties of PropertyType in ComponentClass.

    EditorClass
      The class of the editor to be created whenever a property of the type
      passed in PropertyTypeInfo is displayed in the Object Inspector.  The
      class will be created by calling EditorClass.Create. }

procedure RegisterPropertyEditor(PropertyType: PTypeInfo; ComponentClass: TClass;
  const PropertyName: string; EditorClass: TPropertyEditorClass);

type
  TPropertyMapperFunc = function(Obj: TPersistent;
    PropInfo: PPropInfo): TPropertyEditorClass;

procedure RegisterPropertyMapper(Mapper: TPropertyMapperFunc);

procedure GetComponentProperties(Components: TDesignerSelectionList;
  Filter: TTypeKinds; Designer: IFormDesigner; Proc: TGetPropEditProc);

procedure RegisterComponentEditor(ComponentClass: TComponentClass;
  ComponentEditor: TComponentEditorClass);

function GetComponentEditor(Component: TComponent;
  Designer: IFormDesigner): TComponentEditor;

{ Custom modules }
{ A custom module allows containers that descend from classes other than TForm
  to be created and edited by the form designer. This is useful for other form
  like containers (e.g. a report designer) or for specialized forms (e.g. an
  ActiveForm) or for generic component containers (e.g. a TDataModule). It is
  assumed that the base class registered will call InitInheritedComponent in its
  constructor which will initialize the component from the associated DFM file
  stored in the programs resources. See the constructors of TDataModule and
  TForm for examples of how to write such a constructor.

  The following designer assumptions are made, depending on the base components
  ancestor,

    If ComponentBaseClass descends from TForm,

       it is designed by creating an instance of the component as the form.
       Allows designing TForm descendants and modifying their properties as
       well as the form properties

    If ComponentBaseClass descends from TWinControl (but not TForm),

       it is designed by creating an instance of the control, placing it into a
       design-time form.  The form's client size is the default size of the
       control.

    If ComponentBaseClass descends from TDataModule,

       it is designed by creating an instance of the class and creating a
       special non-visual container designer to edit the components and display
       the icons of the contained components.

  The module will appear in the project file with a colon and the base class
  name appended after the component name (e.g. MyDataModule: TDataModule).

  Note it is not legal to register anything that does not descend from one of
  the above.

  TCustomModule class
    An instance of this class is created for each custom module that is
    loaded. This class is also destroyed whenever the module is unloaded.
    The Saving method is called prior to the file being saved. When the context
    menu for the module is invoked the GetVerbCount and GetVerb methods are
    called to build the menu.  If one of the verbs is selected ExecuteVerb is
    called.
    
    ExecuteVerb(Index)
      The Index'ed verb was selected by the use off the context menu.  The
      meaning of this is determined by custom module.
    GetAttributes
      cmaVirtualSize:  For TWinControl objects only: including this attribute makes
        the control "client aligned" in the design window.  Without this
        attribute, the object is sized independently from the design window.
      Attributes are a set to allow for future expansion of features.
    GetVerb(Index)
      The custom module should return a string that will be displayed in the
      context menu.  It is the responsibility of the custom module to place
      the & character and the '...' characters as appropriate.
    GetVerbCount
      The number of valid indexs to GetVerb and Execute verb.  The index assumed
      to be zero based (i.e. 0..GetVerbCount - 1).
    PrepareItem
      While constructing the context menu PrepareItem will be called for
      each verb.  It will be passed the menu item that will be used to represent
      the verb.  The module editor can customize the menu item as it sees fit,
      including adding subitems.  If you don't want that particular menu item
      to be shown don't free it, simply set it's Visible property to False.
    Saving
      Called prior to the module being saved.
    ValidateComponent(Component)
      ValidateComponent is called whenever a component is created by the
      user for the designer to contain.  The intent is for this procedure to
      raise an exception with a descriptive message if the component is not
      applicable for the container. For example, a TComponent module should
      throw an exception if the component descends from TControl.
    Root
      This is the instance being designed.}

type
  TCustomModuleAttribute = (cmaVirtualSize);
  TCustomModuleAttributes = set of TCustomModuleAttribute;

  TCustomModule = class
  private
    FRoot: IComponent;
  public
    constructor Create(ARoot: IComponent); virtual;
    procedure ExecuteVerb(Index: Integer); virtual;
    function CreateDesignerForm(Designer: IDesigner): TCustomForm; virtual;
    function GetAttributes: TCustomModuleAttributes; virtual;
    function GetVerb(Index: Integer): string; virtual;
    function GetVerbCount: Integer; virtual;
    procedure PrepareItem(Index: Integer; const AItem: TMenuItem); virtual;
    procedure Saving; virtual;
    procedure ValidateComponent(Component: IComponent); virtual;
    class function Nestable: Boolean; virtual;
    property Root: IComponent read FRoot;
  end;

  TCustomModuleClass = class of TCustomModule;

  TRegisterCustomModuleProc = procedure (Group: Integer;
    ComponentBaseClass: TComponentClass;
    CustomModuleClass: TCustomModuleClass);

  ICustomModuleSettings = interface
    ['{50947DAD-E627-11D2-B728-00C04FA35D12}']
    function IniSection: string;
  end;

  ICustomModuleProjectSettings = interface(ICustomModuleSettings)
    ['{78E12CC2-DBCC-11D2-B727-00C04FA35D12}']
    procedure SaveProjectState(AFile: TMemIniFile);
    procedure LoadProjectState(AFile: TMemIniFile);
  end;

  ICustomModuleUnitSettings = interface(ICustomModuleSettings)
    ['{78E12CC1-DBCC-11D2-B727-00C04FA35D12}']
    procedure SaveUnitState(AFile: TMemIniFile);
    procedure LoadUnitState(AFile: TMemIniFile);
  end;

  IDesignerPersistence = interface
    ['{D32194C2-EECF-11D2-AAD2-00C04FB16FBC}']
    procedure Save(const Stream: IStream);
    procedure Load(const Stream: IStream);
  end;

procedure RegisterCustomModule(ComponentBaseClass: TComponentClass;
  CustomModuleClass: TCustomModuleClass);

var
  RegisterCustomModuleProc: TRegisterCustomModuleProc;

{ Routines used by the form designer for package management }

type
  TGroupChangeProc = procedure(AGroup: Integer);
  
function NewEditorGroup: Integer;
procedure FreeEditorGroup(Group: Integer);
procedure NotifyGroupChange(AProc: TGroupChangeProc);
procedure UnNotifyGroupChange(AProc: TGroupChangeProc);

var  // number of significant characters in identifiers
  MaxIdentLength: Byte = 63;

{ Property Categories Classes
  The following three components make up the category management system.
  Access to them is usually managed by the following support functions.

  TPropertyCategoryList
    Contains and maintains the list of TPropertyCategories.  There are numerous
    'As a whole' access and manipulation methods for categories as well as
    simplified access functions.
  TPropertyCategory
    Contains and maintains the list of TPropertyFilters.  There are numerous
    'As a whole' access and manipulation methods for filters as well as data
    about the category itself.
  TPropertyFilter
    Maintains the information about a single filter associated with a particular
    category.  Along with its filter specific data it also encapsulates the
    matching algorithm. }

type
  TPropertyFilter = class(TObject)
  private
    FMask: TMask;
    FComponentClass: TClass;
    FPropertyType: PTypeInfo;
    FGroup: Integer;
  public
    constructor Create(const APropertyName: String; AComponentClass: TClass;
      APropertyType: PTypeInfo);
    destructor Destroy; override;
    function Match(const APropertyName: String; AComponentClass: TClass;
      APropertyType: PTypeInfo): Boolean;
    property ComponentClass: TClass read FComponentClass;
    property PropertyType: PTypeInfo read FPropertyType;
  end;

  TPropertyCategoryClass = class of TPropertyCategory;
  TPropertyCategory = class(TObject)
  private
    FList: TObjectList;
    FMatchCount: Integer;
    FEditor: TPropertyEditor;
    FEnabled, FVisible: Boolean;
    FGroup: Integer;
  protected
    function GetFilter(Index: Integer): TPropertyFilter;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AFilter: TPropertyFilter): TPropertyFilter;
    function Count: integer;
    function Match(const APropertyName: String; AComponentClass: TClass;
      APropertyType: PTypeInfo): Boolean;
    procedure ClearMatches;
    procedure FreeEditorGroup(AGroup: Integer);
    class function Name: string; virtual;
    class function Description: string; virtual;
    procedure PropDraw(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean); dynamic;
    property Filters[Index: Integer]: TPropertyFilter read GetFilter;
    property MatchCount: Integer read FMatchCount;
    property Visible: Boolean read FVisible write FVisible;
    property Editor: TPropertyEditor read FEditor write FEditor;
  end;

  TPropertyCategoryVisibleMode = (pcvAll, pcvToggle, pcvNone, pcvNotListed, pcvOnlyListed);
  TPropertyCategoryList = class(TObject)
  private
    FList: TObjectList;
    FMiscCategory: TPropertyCategory;
  protected
    function GetCategory(Index: Integer): TPropertyCategory;
    function GetHiddenCategories: string;
    procedure SetHiddenCategories(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    function FindCategory(ACategoryClass: TPropertyCategoryClass): TPropertyCategory;
    function IndexOf(ACategoryClass: TPropertyCategoryClass): Integer; overload;
    function IndexOf(const ACategoryName: string): Integer; overload;
    procedure ClearMatches;
    procedure FreeEditorGroup(AGroup: Integer);
    function MiscCategory: TPropertyCategory;
    function Count: integer;
    function Match(const APropertyName: String; AComponentClass: TClass;
      APropertyType: PTypeInfo = nil): Boolean;
    function ChangeVisibility(AMode: TPropertyCategoryVisibleMode): Boolean; overload;
    function ChangeVisibility(AMode: TPropertyCategoryVisibleMode;
      const AClasses: array of TClass): Boolean; overload;
    property HiddenCategories: string read GetHiddenCategories write SetHiddenCategories;
    property Categories[Index: Integer]: TPropertyCategory read GetCategory; default;
  end;

{ Property Categories Helpers

  RegisterPropertyInCategory
    This function comes in four flavors, each taking slightly different set of
    arguments.  You can specify a category filter by property name; by class
    type and property name; by property type and property name; and finally
    just by property type.  Additionally property name may include wild card
    symbols.  For example: you can add all properties that match 'Data*' to
    a particular category.  For a full list of what wild card characters
    are available please refer to the TMask class documentation.
  RegisterPropertiesInCategory
    This function will allow you to register a series of property names and/or
    property types filters in a single statement.
  IsPropertyInCategory
    This function comes in two flavors, each taking a slightly different set of
    arguments.  But in either case you can ask if a property of a certain class
    falls under the specified category.  The class can be specified by name or
    by class type.
  PropertyCategoryList
    This function will return, and create if necessary, the global property
    category list.}

function RegisterPropertyInCategory(ACategoryClass: TPropertyCategoryClass;
  const APropertyName: string): TPropertyFilter; overload;
function RegisterPropertyInCategory(ACategoryClass: TPropertyCategoryClass;
  AComponentClass: TClass; const APropertyName: string): TPropertyFilter; overload;
function RegisterPropertyInCategory(ACategoryClass: TPropertyCategoryClass;
  APropertyType: PTypeInfo; const APropertyName: string): TPropertyFilter; overload;
function RegisterPropertyInCategory(ACategoryClass: TPropertyCategoryClass;
  APropertyType: PTypeInfo): TPropertyFilter; overload;

function RegisterPropertiesInCategory(ACategoryClass: TPropertyCategoryClass;
  const AFilters: array of const): TPropertyCategory; overload;
function RegisterPropertiesInCategory(ACategoryClass: TPropertyCategoryClass;
  AComponentClass: TClass; const AFilters: array of string): TPropertyCategory; overload;
function RegisterPropertiesInCategory(ACategoryClass: TPropertyCategoryClass;
  APropertyType: PTypeInfo; const AFilters: array of string): TPropertyCategory; overload;

function IsPropertyInCategory(ACategoryClass: TPropertyCategoryClass;
  AComponentClass: TClass; const APropertyName: String): Boolean; overload;
function IsPropertyInCategory(ACategoryClass: TPropertyCategoryClass;
  const AClassName: string; const APropertyName: String): Boolean; overload;

function PropertyCategoryList: TPropertyCategoryList;

{ Property Categories
  The following class defines the standard categories used by Delphi.  These are
  general purpose and can be used by component developers for property category
  registration.  Additionally component developers can create new descedents of
  TPropertyCategory to add completly new categories. }

type
  TActionCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TDataCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TDatabaseCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TDragNDropCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  THelpCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TLayoutCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TLegacyCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TLinkageCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TLocaleCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TLocalizableCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TMiscellaneousCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TVisualCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TInputCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

var
  BaseRegistryKey: string = '';

implementation

uses Dialogs, Consts, Registry, Math;

type
  PPropertyClassRec = ^TPropertyClassRec;
  TPropertyClassRec = record
    Group: Integer;
    PropertyType: PTypeInfo;
    PropertyName: string;
    ComponentClass: TClass;
    EditorClass: TPropertyEditorClass;
  end;

  PPropertyMapperRec = ^TPropertyMapperRec;
  TPropertyMapperRec = record
    Group: Integer;
    Mapper: TPropertyMapperFunc;
  end;

const
  PropClassMap: array[TypInfo.TTypeKind] of TPropertyEditorClass = (
    nil, TIntegerProperty, TCharProperty, TEnumProperty,
    TFloatProperty, TStringProperty, TSetProperty, TClassProperty,
    TMethodProperty, TPropertyEditor, TStringProperty, TStringProperty,
    TPropertyEditor, nil, nil, nil, TInt64Property, nil);
      (* tkArray, tkRecord, kInterface, tkInt64, tkDynArray *)

var
  PropertyClassList: TList = nil;
  EditorGroupList: TBits = nil;
  PropertyMapperList: TList = nil;
  InternalPropertyCategoryList: TPropertyCategoryList = nil;

const

  { context ids for the Font editor and the Color Editor, etc. }
  hcDFontEditor       = 25000;
  hcDColorEditor      = 25010;
  hcDMediaPlayerOpen  = 25020;

{ TDesignerSelectionList }

constructor TDesignerSelectionList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TDesignerSelectionList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TDesignerSelectionList.Get(Index: Integer): TPersistent;
begin
  Result := FList[Index];
end;

function TDesignerSelectionList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDesignerSelectionList.Add(Item: TPersistent): Integer;
begin
  Result := FList.Add(Item);
end;

function TDesignerSelectionList.Equals(List: TDesignerSelectionList): Boolean;
var
  I: Integer;
begin
  Result := False;
  if List.Count <> FList.Count then Exit;
  for I := 0 to List.Count - 1 do if List[I] <> FList[I] then Exit;
  Result := True;
end;

function TDesignerSelectionList.Intf_Add(const Item: IPersistent): Integer;
begin
  Result := Add(ExtractPersistent(Item));
end;

function TDesignerSelectionList.Intf_Equals(const List: IDesignerSelections): Boolean;
var
  I: Integer;
  CompList: IComponentList;
  P1, P2: IPersistent;
begin
  if List.QueryInterface(IComponentList, CompList) = 0 then
    Result := CompList.GetComponentList.Equals(Self)
  else
  begin
    Result := False;
    if List.Count <> FList.Count then Exit;
    for I := 0 to List.Count - 1 do
    begin
      P1 := Intf_Get(I);
      P2 := List[I];
      if ((P1 = nil) and (P2 <> nil)) or
        (P2 = nil) or not P1.Equals(P2) then Exit;
    end;
    Result := True;
  end;
end;

function TDesignerSelectionList.Intf_Get(Index: Integer): IPersistent;
begin
  Result := MakeIPersistent(TPersistent(FList[Index]));
end;

function TDesignerSelectionList.GetComponentList: TDesignerSelectionList;
begin
  Result := Self;
end;

{ TPropertyEditor }

constructor TPropertyEditor.Create(const ADesigner: IFormDesigner;
  APropCount: Integer);
begin
  FDesigner := ADesigner;
  GetMem(FPropList, APropCount * SizeOf(TInstProp));
  FPropCount := APropCount;
end;

destructor TPropertyEditor.Destroy;
begin
  if FPropList <> nil then
    FreeMem(FPropList, FPropCount * SizeOf(TInstProp));
end;

procedure TPropertyEditor.Activate;
begin
end;

function TPropertyEditor.AllEqual: Boolean;
begin
  Result := FPropCount = 1;
end;

procedure TPropertyEditor.Edit;
type
  TGetStrFunc = function(const Value: string): Integer of object;
var
  I: Integer;
  Values: TStringList;
  AddValue: TGetStrFunc;
begin
  if not AutoFill then Exit;
  Values := TStringList.Create;
  Values.Sorted := paSortList in GetAttributes;
  try
    AddValue := Values.Add;
    GetValues(TGetStrProc(AddValue));
    if Values.Count > 0 then
    begin
      I := Values.IndexOf(Value) + 1;
      if I = Values.Count then I := 0;
      Value := Values[I];
    end;
  finally
    Values.Free;
  end;
end;

function TPropertyEditor.AutoFill: Boolean;
begin
  Result := True;
end;

function TPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
end;

function TPropertyEditor.GetComponent(Index: Integer): TPersistent;
begin
  Result := FPropList^[Index].Instance;
end;

function TPropertyEditor.GetFloatValue: Extended;
begin
  Result := GetFloatValueAt(0);
end;

function TPropertyEditor.GetFloatValueAt(Index: Integer): Extended;
begin
  with FPropList^[Index] do Result := GetFloatProp(Instance, PropInfo);
end;

function TPropertyEditor.GetMethodValue: TMethod;
begin
  Result := GetMethodValueAt(0);
end;

function TPropertyEditor.GetMethodValueAt(Index: Integer): TMethod;
begin
  with FPropList^[Index] do Result := GetMethodProp(Instance, PropInfo);
end;

function TPropertyEditor.GetEditLimit: Integer;
begin
  Result := 255;
end;

function TPropertyEditor.GetName: string;
begin
  Result := FPropList^[0].PropInfo^.Name;
end;

function TPropertyEditor.GetOrdValue: Longint;
begin
  Result := GetOrdValueAt(0);
end;

function TPropertyEditor.GetOrdValueAt(Index: Integer): Longint;
begin
  with FPropList^[Index] do Result := GetOrdProp(Instance, PropInfo);
end;

function TPropertyEditor.GetPrivateDirectory: string;
begin
  Result := '';
  if Designer <> nil then
    Result := Designer.GetPrivateDirectory;
end;

procedure TPropertyEditor.GetProperties(Proc: TGetPropEditProc);
begin
end;

function TPropertyEditor.GetPropInfo: PPropInfo;
begin
  Result := FPropList^[0].PropInfo;
end;

function TPropertyEditor.GetPropType: PTypeInfo;
begin
  Result := FPropList^[0].PropInfo^.PropType^;
end;

function TPropertyEditor.GetStrValue: string;
begin
  Result := GetStrValueAt(0);
end;

function TPropertyEditor.GetStrValueAt(Index: Integer): string;
begin
  with FPropList^[Index] do Result := GetStrProp(Instance, PropInfo);
end;

function TPropertyEditor.GetVarValue: Variant;
begin
  Result := GetVarValueAt(0);
end;

function TPropertyEditor.GetVarValueAt(Index: Integer): Variant;
begin
  with FPropList^[Index] do Result := GetVariantProp(Instance, PropInfo);
end;

function TPropertyEditor.GetValue: string;
begin
  Result := srUnknown;
end;

function TPropertyEditor.GetVisualValue: string;
begin
  if AllEqual then
    Result := GetValue
  else
    Result := '';
end;

procedure TPropertyEditor.GetValues(Proc: TGetStrProc);
begin
end;

procedure TPropertyEditor.Initialize;
begin
end;

procedure TPropertyEditor.Modified;
begin
  if Designer <> nil then
    Designer.Modified;
end;

procedure TPropertyEditor.SetFloatValue(Value: Extended);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do SetFloatProp(Instance, PropInfo, Value);
  Modified;
end;

procedure TPropertyEditor.SetMethodValue(const Value: TMethod);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do SetMethodProp(Instance, PropInfo, Value);
  Modified;
end;

procedure TPropertyEditor.SetOrdValue(Value: Longint);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do SetOrdProp(Instance, PropInfo, Value);
  Modified;
end;

procedure TPropertyEditor.SetPropEntry(Index: Integer;
  AInstance: TPersistent; APropInfo: PPropInfo);
begin
  with FPropList^[Index] do
  begin
    Instance := AInstance;
    PropInfo := APropInfo;
  end;
end;

procedure TPropertyEditor.SetStrValue(const Value: string);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do SetStrProp(Instance, PropInfo, Value);
  Modified;
end;

procedure TPropertyEditor.SetVarValue(const Value: Variant);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do SetVariantProp(Instance, PropInfo, Value);
  Modified;
end;

procedure TPropertyEditor.Revert;
var
  I: Integer;
begin
  if Designer <> nil then
    for I := 0 to FPropCount - 1 do
      with FPropList^[I] do Designer.Revert(Instance, PropInfo);
end;

procedure TPropertyEditor.SetValue(const Value: string);
begin
end;

function TPropertyEditor.ValueAvailable: Boolean;
var
  I: Integer;
  S: string;
begin
  Result := True;
  for I := 0 to FPropCount - 1 do
  begin
    if (FPropList^[I].Instance is TComponent) and
      (csCheckPropAvail in TComponent(FPropList^[I].Instance).ComponentStyle) then
    begin
      try
        S := GetValue;
        AllEqual;
      except
        Result := False;
      end;
      Exit;
    end;
  end;
end;

function TPropertyEditor.GetInt64Value: Int64;
begin
  Result := GetInt64ValueAt(0);
end;

function TPropertyEditor.GetInt64ValueAt(Index: Integer): Int64;
begin
  with FPropList^[Index] do Result := GetInt64Prop(Instance, PropInfo);
end;

procedure TPropertyEditor.SetInt64Value(Value: Int64);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do SetInt64Prop(Instance, PropInfo, Value);
  Modified;
end;

{ these three procedures implement the default render behavior of the
  object/property inspector's drop down list editor.  you don't need to
  override the two measure procedures if the default width or height don't
  need to be changed. }
procedure TPropertyEditor.ListMeasureHeight(const Value: string; ACanvas: TCanvas;
  var AHeight: Integer);
begin
end;

procedure TPropertyEditor.ListMeasureWidth(const Value: string; ACanvas: TCanvas;
  var AWidth: Integer);
begin
end;

procedure TPropertyEditor.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  ACanvas.TextRect(ARect, ARect.Left + 1, ARect.Top + 1, Value);
end;

{ these two procedures implement the default render behavior of the
  object/property inspector }
procedure TPropertyEditor.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  ACanvas.TextRect(ARect, ARect.Left + 1, ARect.Top + 1, GetName);
end;

procedure TPropertyEditor.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  ACanvas.TextRect(ARect, ARect.Left + 1, ARect.Top + 1, GetVisualValue)
end;

{ TOrdinalProperty }

function TOrdinalProperty.AllEqual: Boolean;
var
  I: Integer;
  V: Longint;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetOrdValue;
    for I := 1 to PropCount - 1 do
      if GetOrdValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TOrdinalProperty.GetEditLimit: Integer;
begin
  Result := 63;
end;

{ TIntegerProperty }

function TIntegerProperty.GetValue: string;
begin
  with GetTypeData(GetPropType)^ do
    if OrdType = otULong then // unsigned
      Result := IntToStr(Cardinal(GetOrdValue))
    else
      Result := IntToStr(GetOrdValue);
end;

procedure TIntegerProperty.SetValue(const Value: String);

  procedure Error(const Args: array of const);
  begin
    raise EPropertyError.CreateResFmt(@SOutOfRange, Args);
  end;

var
  L: Int64;
begin
  L := StrToInt64(Value);
  with GetTypeData(GetPropType)^ do
    if OrdType = otULong then
    begin   // unsigned compare and reporting needed
      if (L < Cardinal(MinValue)) or (L > Cardinal(MaxValue)) then
        // bump up to Int64 to get past the %d in the format string
        Error([Int64(Cardinal(MinValue)), Int64(Cardinal(MaxValue))]);
    end
    else if (L < MinValue) or (L > MaxValue) then
      Error([MinValue, MaxValue]);
  SetOrdValue(L);
end;

{ TCharProperty }

function TCharProperty.GetValue: string;
var
  Ch: Char;
begin
  Ch := Chr(GetOrdValue);
  if Ch in [#33..#127] then
    Result := Ch else
    FmtStr(Result, '#%d', [Ord(Ch)]);
end;

procedure TCharProperty.SetValue(const Value: string);
var
  L: Longint;
begin
  if Length(Value) = 0 then L := 0 else
    if Length(Value) = 1 then L := Ord(Value[1]) else
      if Value[1] = '#' then L := StrToInt(Copy(Value, 2, Maxint)) else
        raise EPropertyError.CreateRes(@SInvalidPropertyValue);
  with GetTypeData(GetPropType)^ do
    if (L < MinValue) or (L > MaxValue) then
      raise EPropertyError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
  SetOrdValue(L);
end;

{ TEnumProperty }

function TEnumProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

function TEnumProperty.GetValue: string;
var
  L: Longint;
begin
  L := GetOrdValue;
  with GetTypeData(GetPropType)^ do
    if (L < MinValue) or (L > MaxValue) then L := MaxValue;
  Result := GetEnumName(GetPropType, L);
end;

procedure TEnumProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  EnumType: PTypeInfo;
begin
  EnumType := GetPropType;
  with GetTypeData(EnumType)^ do
    for I := MinValue to MaxValue do Proc(GetEnumName(EnumType, I));
end;

procedure TEnumProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  I := GetEnumValue(GetPropType, Value);
  if I < 0 then raise EPropertyError.CreateRes(@SInvalidPropertyValue);
  SetOrdValue(I);
end;

{ TBoolProperty  }

function TBoolProperty.GetValue: string;
begin
  if GetOrdValue = 0 then
    Result := 'False'
  else
    Result := 'True';
end;

procedure TBoolProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('False');
  Proc('True');
end;

procedure TBoolProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  if CompareText(Value, 'False') = 0 then
    I := 0
  else if CompareText(Value, 'True') = 0 then
    I := -1
  else
    I := StrToInt(Value);
  SetOrdValue(I);
end;

{ TInt64Property }

function TInt64Property.AllEqual: Boolean;
var
  I: Integer;
  V: Int64;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetInt64Value;
    for I := 1 to PropCount - 1 do
      if GetInt64ValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TInt64Property.GetEditLimit: Integer;
begin
  Result := 63;
end;

function TInt64Property.GetValue: string;
begin
  Result := IntToStr(GetInt64Value);
end;

procedure TInt64Property.SetValue(const Value: string);
begin
  SetInt64Value(StrToInt64(Value));
end;


{ TFloatProperty }

function TFloatProperty.AllEqual: Boolean;
var
  I: Integer;
  V: Extended;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetFloatValue;
    for I := 1 to PropCount - 1 do
      if GetFloatValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TFloatProperty.GetValue: string;
const
  Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 18);
begin
  Result := FloatToStrF(GetFloatValue, ffGeneral,
    Precisions[GetTypeData(GetPropType)^.FloatType], 0);
end;

procedure TFloatProperty.SetValue(const Value: string);
begin
  SetFloatValue(StrToFloat(Value));
end;

{ TStringProperty }

function TStringProperty.AllEqual: Boolean;
var
  I: Integer;
  V: string;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetStrValue;
    for I := 1 to PropCount - 1 do
      if GetStrValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TStringProperty.GetEditLimit: Integer;
begin
  if GetPropType^.Kind = tkString then
    Result := GetTypeData(GetPropType)^.MaxLength else
    Result := 255;
end;

function TStringProperty.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TStringProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

{ TComponentNameProperty }

function TComponentNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [];
end;

function TComponentNameProperty.GetEditLimit: Integer;
begin
  Result := MaxIdentLength;
end;

{ TNestedProperty }

constructor TNestedProperty.Create(Parent: TPropertyEditor);
begin
  FDesigner := Parent.Designer;
  FPropList := Parent.FPropList;
  FPropCount := Parent.PropCount;
end;

destructor TNestedProperty.Destroy;
begin
end;

{ TSetElementProperty }

constructor TSetElementProperty.Create(Parent: TPropertyEditor; AElement: Integer);
begin
  inherited Create(Parent);
  FElement := AElement;
end;

function TSetElementProperty.AllEqual: Boolean;
var
  I: Integer;
  S: TIntegerSet;
  V: Boolean;
begin
  Result := False;
  if PropCount > 1 then
  begin
    Integer(S) := GetOrdValue;
    V := FElement in S;
    for I := 1 to PropCount - 1 do
    begin
      Integer(S) := GetOrdValueAt(I);
      if (FElement in S) <> V then Exit;
    end;
  end;
  Result := True;
end;

function TSetElementProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList];
end;

function TSetElementProperty.GetName: string;
begin
  Result := GetEnumName(GetTypeData(GetPropType)^.CompType^, FElement);
end;

function TSetElementProperty.GetValue: string;
var
  S: TIntegerSet;
begin
  Integer(S) := GetOrdValue;
  Result := BooleanIdents[FElement in S];
end;

procedure TSetElementProperty.GetValues(Proc: TGetStrProc);
begin
  Proc(BooleanIdents[False]);
  Proc(BooleanIdents[True]);
end;

procedure TSetElementProperty.SetValue(const Value: string);
var
  S: TIntegerSet;
begin
  Integer(S) := GetOrdValue;
  if CompareText(Value, 'True') = 0 then
    Include(S, FElement) else
    Exclude(S, FElement);
  SetOrdValue(Integer(S));
end;

{ TSetProperty }

function TSetProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly, paRevertable];
end;

procedure TSetProperty.GetProperties(Proc: TGetPropEditProc);
var
  I: Integer;
begin
  with GetTypeData(GetTypeData(GetPropType)^.CompType^)^ do
    for I := MinValue to MaxValue do
      Proc(TSetElementProperty.Create(Self, I));
end;

function TSetProperty.GetValue: string;
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Integer(S) := GetOrdValue;
  TypeInfo := GetTypeData(GetPropType)^.CompType^;
  Result := '[';
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Length(Result) <> 1 then Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  Result := Result + ']';
end;

{ TClassProperty }

function TClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly];
end;

procedure TClassProperty.GetProperties(Proc: TGetPropEditProc);
var
  I: Integer;
  Components: TDesignerSelectionList;
begin
  Components := TDesignerSelectionList.Create;
  try
    for I := 0 to PropCount - 1 do
      Components.Add(TComponent(GetOrdValueAt(I)));
    GetComponentProperties(Components, tkProperties, Designer, Proc);
  finally
    Components.Free;
  end;
end;

function TClassProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

{ TComponentProperty }

procedure TComponentProperty.Edit;
begin
  if (GetKeyState(VK_CONTROL) < 0) and
     (GetKeyState(VK_LBUTTON) < 0) and
     (GetOrdValue <> 0) then
    Designer.SelectComponent(TPersistent(GetOrdValue))
  else
    inherited Edit;
end;

function TComponentProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

function TComponentProperty.GetEditLimit: Integer;
begin
  Result := 127;
end;

function TComponentProperty.GetValue: string;
begin
  Result := Designer.GetComponentName(TComponent(GetOrdValue));
end;

procedure TComponentProperty.GetValues(Proc: TGetStrProc);
begin
  Designer.GetComponentNames(GetTypeData(GetPropType), Proc);
end;

procedure TComponentProperty.SetValue(const Value: string);
var
  Component: TComponent;
begin
  if Value = '' then Component := nil else
  begin
    Component := Designer.GetComponent(Value);
    if not (Component is GetTypeData(GetPropType)^.ClassType) then
      raise EPropertyError.CreateRes(@SInvalidPropertyValue);
  end;
  SetOrdValue(Longint(Component));
end;

{ TMethodProperty }

function TMethodProperty.AllEqual: Boolean;
var
  I: Integer;
  V, T: TMethod;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetMethodValue;
    for I := 1 to PropCount - 1 do
    begin
      T := GetMethodValueAt(I);
      if (T.Code <> V.Code) or (T.Data <> V.Data) then Exit;
    end;
  end;
  Result := True;
end;

procedure TMethodProperty.Edit;
var
  FormMethodName: string;
begin
  FormMethodName := GetValue;
  if (FormMethodName = '') or
    Designer.MethodFromAncestor(GetMethodValue) then
  begin
    if FormMethodName = '' then
      FormMethodName := GetFormMethodName;
    if FormMethodName = '' then
      raise EPropertyError.CreateRes(@SCannotCreateName);
    SetValue(FormMethodName);
  end;
  Designer.ShowMethod(FormMethodName);
end;

function TMethodProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

function TMethodProperty.GetEditLimit: Integer;
begin
  Result := MaxIdentLength;
end;

function TMethodProperty.GetFormMethodName: string;
var
  I: Integer;
begin
  if GetComponent(0) = Designer.GetRoot then
  begin
    Result := Designer.GetRootClassName;
    if (Result <> '') and (Result[1] = 'T') then
      Delete(Result, 1, 1);
  end
  else
  begin
    Result := Designer.GetObjectName(GetComponent(0));
    for I := Length(Result) downto 1 do
      if Result[I] in ['.','[',']','-','>'] then
        Delete(Result, I, 1);
  end;
  if Result = '' then
    raise EPropertyError.CreateRes(@SCannotCreateName);
  Result := Result + GetTrimmedEventName;
end;

function TMethodProperty.GetTrimmedEventName: string;
begin
  Result := GetName;
  if (Length(Result) >= 2) and
    (Result[1] in ['O','o']) and (Result[2] in ['N','n']) then
    Delete(Result,1,2);
end;

function TMethodProperty.GetValue: string;
begin
  Result := Designer.GetMethodName(GetMethodValue);
end;

procedure TMethodProperty.GetValues(Proc: TGetStrProc);
begin
  Designer.GetMethods(GetTypeData(GetPropType), Proc);
end;

procedure TMethodProperty.SetValue(const AValue: string);

  procedure CheckChainCall(const MethodName: string; Method: TMethod);
  var
    Persistent: TPersistent;
    Component: TComponent;
    InstanceMethod: string;
    Instance: TComponent;
  begin
    Persistent := GetComponent(0);
    if Persistent is TComponent then
    begin
      Component := TComponent(Persistent);
      if (Component.Name <> '') and (Method.Data <> Designer.GetRoot) and
        (TObject(Method.Data) is TComponent) then
      begin
        Instance := TComponent(Method.Data);
        InstanceMethod := Instance.MethodName(Method.Code);
        if InstanceMethod <> '' then
          Designer.ChainCall(MethodName, Instance.Name, InstanceMethod,
            GetTypeData(GetPropType));
      end;
    end;
  end;

var
  NewMethod: Boolean;
  CurValue: string;
  OldMethod: TMethod;
begin
  CurValue:= GetValue;
  if (CurValue <> '') and (AValue <> '') and (SameText(CurValue, AValue) or
    not Designer.MethodExists(AValue)) and not Designer.MethodFromAncestor(GetMethodValue) then
    Designer.RenameMethod(CurValue, AValue)
  else
  begin
    NewMethod := (AValue <> '') and not Designer.MethodExists(AValue);
    OldMethod := GetMethodValue;
    SetMethodValue(Designer.CreateMethod(AValue, GetTypeData(GetPropType)));
    if NewMethod then
    begin
      if (PropCount = 1) and (OldMethod.Data <> nil) and (OldMethod.Code <> nil) then
        CheckChainCall(AValue, OldMethod);
      Designer.ShowMethod(AValue);
    end;
  end;
end;

{ TFontNameProperty }
{ Owner draw code has been commented out, see the interface section's for info. }

function TFontNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TFontNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to Screen.Fonts.Count - 1 do Proc(Screen.Fonts[I]);
end;

procedure TFontNameProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  vOldFontName: string;
begin
  if FontNamePropertyDisplayFontNames then
    with ACanvas do
    begin
      // save off things
      vOldFontName := Font.Name;

      // set things up and do work
      Font.Name := Value;
      TextRect(ARect, ARect.Left + 2, ARect.Top + 1, Value);

      // restore things
      Font.Name := vOldFontName;
    end
  else
    inherited ListDrawValue(Value, ACanvas, ARect, ASelected);
end;

procedure TFontNameProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  vOldFontName: string;
begin
  if FontNamePropertyDisplayFontNames then
    with ACanvas do
    begin
      // save off things
      vOldFontName := Font.Name;

      // set things up and do work
      Font.Name := Value;
      AHeight := TextHeight(Value) + 2;

      // restore things
      Font.Name := vOldFontName;
    end
  else
    inherited ListMeasureHeight(Value, ACanvas, AHeight);
end;

procedure TFontNameProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  vOldFontName: string;
begin
  if FontNamePropertyDisplayFontNames then
    with ACanvas do
    begin
      // save off things
      vOldFontName := Font.Name;

      // set things up and do work
      Font.Name := Value;
      AWidth := TextWidth(Value) + 4;

      // restore things
      Font.Name := vOldFontName;
    end
  else
    inherited ListMeasureWidth(Value, ACanvas, AWidth);
end;

{ TFontCharsetProperty }

function TFontCharsetProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSortList, paValueList];
end;

function TFontCharsetProperty.GetValue: string;
begin
  if not CharsetToIdent(TFontCharset(GetOrdValue), Result) then
    FmtStr(Result, '%d', [GetOrdValue]);
end;

procedure TFontCharsetProperty.GetValues(Proc: TGetStrProc);
begin
  GetCharsetValues(Proc);
end;

procedure TFontCharsetProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToCharset(Value, NewValue) then
    SetOrdValue(NewValue)
  else inherited SetValue(Value);
end;

{ TImeNameProperty }

function TImeNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TImeNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to Screen.Imes.Count - 1 do Proc(Screen.Imes[I]);
end;

{ TMPFilenameProperty }

procedure TMPFilenameProperty.Edit;
var
  MPFileOpen: TOpenDialog;
begin
  MPFileOpen := TOpenDialog.Create(Application);
  MPFileOpen.Filename := GetValue;
  MPFileOpen.Filter := SMPOpenFilter;
  MPFileOpen.HelpContext := hcDMediaPlayerOpen;
  MPFileOpen.Options := MPFileOpen.Options + [ofShowHelp, ofPathMustExist,
    ofFileMustExist];
  try
    if MPFileOpen.Execute then SetValue(MPFileOpen.Filename);
  finally
    MPFileOpen.Free;
  end;
end;

function TMPFilenameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

{ TColorProperty }

procedure TColorProperty.Edit;
var
  ColorDialog: TColorDialog;
  IniFile: TRegIniFile;

  procedure GetCustomColors;
  begin
    if BaseRegistryKey = '' then Exit;
    IniFile := TRegIniFile.Create(BaseRegistryKey);
    try
      IniFile.ReadSectionValues(SCustomColors, ColorDialog.CustomColors);
    except
      { Ignore errors reading values }
    end;
  end;

  procedure SaveCustomColors;
  var
    I, P: Integer;
    S: string;
  begin
    if IniFile <> nil then
      with ColorDialog do
        for I := 0 to CustomColors.Count - 1 do
        begin
          S := CustomColors.Strings[I];
          P := Pos('=', S);
          if P <> 0 then
          begin
            S := Copy(S, 1, P - 1);
            IniFile.WriteString(SCustomColors, S,
              CustomColors.Values[S]);
          end;
        end;
  end;

begin
  IniFile := nil;
  ColorDialog := TColorDialog.Create(Application);
  try
    GetCustomColors;
    ColorDialog.Color := GetOrdValue;
    ColorDialog.HelpContext := hcDColorEditor;
    ColorDialog.Options := [cdShowHelp];
    if ColorDialog.Execute then SetOrdValue(ColorDialog.Color);
    SaveCustomColors;
  finally
    IniFile.Free;
    ColorDialog.Free;
  end;
end;

function TColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

function TColorProperty.GetValue: string;
begin
  Result := ColorToString(TColor(GetOrdValue));
end;

procedure TColorProperty.GetValues(Proc: TGetStrProc);
begin
  GetColorValues(Proc);
end;

procedure TColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, True{ASelected})
  else
    inherited PropDrawValue(ACanvas, ARect, ASelected);
end;

procedure TColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
  function ColorToBorderColor(AColor: TColor): TColor;
  type
    TColorQuad = record
      Red,
      Green,
      Blue,
      Alpha: Byte;
    end;
  begin
    if (TColorQuad(AColor).Red > 192) or
       (TColorQuad(AColor).Green > 192) or
       (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else if ASelected then
      Result := clWhite
    else
      Result := AColor;
  end;
var
  vRight: Integer;
  vOldPenColor, vOldBrushColor: TColor;
begin
  vRight := (ARect.Bottom - ARect.Top) {* 2} + ARect.Left;
  with ACanvas do
  try
    // save off things
    vOldPenColor := Pen.Color;
    vOldBrushColor := Brush.Color;

    // frame things
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);

    // set things up and do the work
    Brush.Color := StringToColor(Value);
    Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);

    // restore the things we twiddled with
    Brush.Color := vOldBrushColor;
    Pen.Color := vOldPenColor;
  finally
    inherited ListDrawValue(Value, ACanvas,
                            Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom),
                            ASelected);
  end;
end;

procedure TColorProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('M') {* 2};
end;

procedure TColorProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToColor(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;

{ TBrushStyleProperty }

procedure TBrushStyleProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, ASelected)
  else
    inherited PropDrawValue(ACanvas, ARect, ASelected);
end;

procedure TBrushStyleProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  vRight: Integer;
  vOldPenColor, vOldBrushColor: TColor;
  vOldBrushStyle: TBrushStyle;
begin
  vRight := (ARect.Bottom - ARect.Top) {* 2} + ARect.Left;
  with ACanvas do
  try
    // save off things
    vOldPenColor := Pen.Color;
    vOldBrushColor := Brush.Color;
    vOldBrushStyle := Brush.Style;

    // frame things
    Pen.Color := Brush.Color;
    Brush.Color := clWindow;
    Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);

    // set things up
    Pen.Color := clWindowText;
    Brush.Style := TBrushStyle(GetEnumValue(GetPropInfo^.PropType^, Value));

    // bsClear hack
    if Brush.Style = bsClear then
    begin
      Brush.Color := clWindow;
      Brush.Style := bsSolid;
    end
    else
      Brush.Color := clWindowText;

    // ok on with the show
    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);

    // restore the things we twiddled with
    Brush.Color := vOldBrushColor;
    Brush.Style := vOldBrushStyle;
    Pen.Color := vOldPenColor;
  finally
    inherited ListDrawValue(Value, ACanvas,
                            Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom),
                            ASelected);
  end;
end;

procedure TBrushStyleProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('A') {* 2};
end;

{ TPenStyleProperty }

procedure TPenStyleProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, ASelected)
  else
    inherited PropDrawValue(ACanvas, ARect, ASelected);
end;

procedure TPenStyleProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  vRight, vTop: Integer;
  vOldPenColor, vOldBrushColor: TColor;
  vOldPenStyle: TPenStyle;
begin
  vRight := (ARect.Bottom - ARect.Top) * 2 + ARect.Left;
  vTop := (ARect.Bottom - ARect.Top) div 2 + ARect.Top;
  with ACanvas do
  try
    // save off things
    vOldPenColor := Pen.Color;
    vOldBrushColor := Brush.Color;
    vOldPenStyle := Pen.Style;

    // frame things
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);

    // white out the background
    Pen.Color := clWindowText;
    Brush.Color := clWindow;
    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);

    // set thing up and do work
    Pen.Color := clWindowText;
    Pen.Style := TPenStyle(GetEnumValue(GetPropInfo^.PropType^, Value));
    MoveTo(ARect.Left + 1, vTop);
    LineTo(vRight - 1, vTop);
    MoveTo(ARect.Left + 1, vTop + 1);
    LineTo(vRight - 1, vTop + 1);

    // restore the things we twiddled with
    Brush.Color := vOldBrushColor;
    Pen.Style := vOldPenStyle;
    Pen.Color := vOldPenColor;
  finally
    inherited ListDrawValue(Value, ACanvas,
                            Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom),
                            ASelected);
  end;
end;

procedure TPenStyleProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('X') * 2;
end;

{ TCursorProperty }

function TCursorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

function TCursorProperty.GetValue: string;
begin
  Result := CursorToString(TCursor(GetOrdValue));
end;

procedure TCursorProperty.GetValues(Proc: TGetStrProc);
begin
  GetCursorValues(Proc);
end;

procedure TCursorProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToCursor(Value, NewValue) then
    SetOrdValue(NewValue)
  else inherited SetValue(Value);
end;

procedure TCursorProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  vRight: Integer;
  CursorIndex: Integer;
  CursorHandle: THandle;
begin
  vRight := ARect.Left + GetSystemMetrics(SM_CXCURSOR) + 4;
  with ACanvas do
  try
    if not IdentToCursor(Value, CursorIndex) then
      CursorIndex := StrToInt(Value);
    ACanvas.FillRect(ARect);
    CursorHandle := Screen.Cursors[CursorIndex];
    if CursorHandle <> 0 then
      DrawIconEx(ACanvas.Handle, ARect.Left + 2, ARect.Top + 2, CursorHandle,
        0, 0, 0, 0, DI_NORMAL or DI_DEFAULTSIZE);
  finally
    inherited ListDrawValue(Value, ACanvas,
                            Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom),
                            ASelected);
  end;
end;

procedure TCursorProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + GetSystemMetrics(SM_CXCURSOR) + 4;
end;

procedure TCursorProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := Max(ACanvas.TextHeight('Wg'), GetSystemMetrics(SM_CYCURSOR) + 4);
end;

{ TFontProperty }

procedure TFontProperty.Edit;
var
  FontDialog: TFontDialog;
begin
  FontDialog := TFontDialog.Create(Application);
  try
    FontDialog.Font := TFont(GetOrdValue);
    FontDialog.HelpContext := hcDFontEditor;
    FontDialog.Options := FontDialog.Options + [fdShowHelp, fdForceFontExist];
    if FontDialog.Execute then SetOrdValue(Longint(FontDialog.Font));
  finally
    FontDialog.Free;
  end;
end;

function TFontProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paDialog, paReadOnly];
end;

{ TModalResultProperty }

const
  ModalResults: array[mrNone..mrYesToAll] of string = (
    'mrNone',
    'mrOk',
    'mrCancel',
    'mrAbort',
    'mrRetry',
    'mrIgnore',
    'mrYes',
    'mrNo',
    'mrAll',
    'mrNoToAll',
    'mrYesToAll');

function TModalResultProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

function TModalResultProperty.GetValue: string;
var
  CurValue: Longint;
begin
  CurValue := GetOrdValue;
  case CurValue of
    Low(ModalResults)..High(ModalResults):
      Result := ModalResults[CurValue];
  else
    Result := IntToStr(CurValue);
  end;
end;

procedure TModalResultProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(ModalResults) to High(ModalResults) do Proc(ModalResults[I]);
end;

procedure TModalResultProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  if Value = '' then
  begin
    SetOrdValue(0);
    Exit;
  end;
  for I := Low(ModalResults) to High(ModalResults) do
    if CompareText(ModalResults[I], Value) = 0 then
    begin
      SetOrdValue(I);
      Exit;
    end;
  inherited SetValue(Value);
end;

{ TShortCutProperty }

const
  ShortCuts: array[0..108] of TShortCut = (
    scNone,
    Byte('A') or scCtrl,
    Byte('B') or scCtrl,
    Byte('C') or scCtrl,
    Byte('D') or scCtrl,
    Byte('E') or scCtrl,
    Byte('F') or scCtrl,
    Byte('G') or scCtrl,
    Byte('H') or scCtrl,
    Byte('I') or scCtrl,
    Byte('J') or scCtrl,
    Byte('K') or scCtrl,
    Byte('L') or scCtrl,
    Byte('M') or scCtrl,
    Byte('N') or scCtrl,
    Byte('O') or scCtrl,
    Byte('P') or scCtrl,
    Byte('Q') or scCtrl,
    Byte('R') or scCtrl,
    Byte('S') or scCtrl,
    Byte('T') or scCtrl,
    Byte('U') or scCtrl,
    Byte('V') or scCtrl,
    Byte('W') or scCtrl,
    Byte('X') or scCtrl,
    Byte('Y') or scCtrl,
    Byte('Z') or scCtrl,
    Byte('A') or scCtrl or scAlt,
    Byte('B') or scCtrl or scAlt,
    Byte('C') or scCtrl or scAlt,
    Byte('D') or scCtrl or scAlt,
    Byte('E') or scCtrl or scAlt,
    Byte('F') or scCtrl or scAlt,
    Byte('G') or scCtrl or scAlt,
    Byte('H') or scCtrl or scAlt,
    Byte('I') or scCtrl or scAlt,
    Byte('J') or scCtrl or scAlt,
    Byte('K') or scCtrl or scAlt,
    Byte('L') or scCtrl or scAlt,
    Byte('M') or scCtrl or scAlt,
    Byte('N') or scCtrl or scAlt,
    Byte('O') or scCtrl or scAlt,
    Byte('P') or scCtrl or scAlt,
    Byte('Q') or scCtrl or scAlt,
    Byte('R') or scCtrl or scAlt,
    Byte('S') or scCtrl or scAlt,
    Byte('T') or scCtrl or scAlt,
    Byte('U') or scCtrl or scAlt,
    Byte('V') or scCtrl or scAlt,
    Byte('W') or scCtrl or scAlt,
    Byte('X') or scCtrl or scAlt,
    Byte('Y') or scCtrl or scAlt,
    Byte('Z') or scCtrl or scAlt,
    VK_F1,
    VK_F2,
    VK_F3,
    VK_F4,
    VK_F5,
    VK_F6,
    VK_F7,
    VK_F8,
    VK_F9,
    VK_F10,
    VK_F11,
    VK_F12,
    VK_F1 or scCtrl,
    VK_F2 or scCtrl,
    VK_F3 or scCtrl,
    VK_F4 or scCtrl,
    VK_F5 or scCtrl,
    VK_F6 or scCtrl,
    VK_F7 or scCtrl,
    VK_F8 or scCtrl,
    VK_F9 or scCtrl,
    VK_F10 or scCtrl,
    VK_F11 or scCtrl,
    VK_F12 or scCtrl,
    VK_F1 or scShift,
    VK_F2 or scShift,
    VK_F3 or scShift,
    VK_F4 or scShift,
    VK_F5 or scShift,
    VK_F6 or scShift,
    VK_F7 or scShift,
    VK_F8 or scShift,
    VK_F9 or scShift,
    VK_F10 or scShift,
    VK_F11 or scShift,
    VK_F12 or scShift,
    VK_F1 or scShift or scCtrl,
    VK_F2 or scShift or scCtrl,
    VK_F3 or scShift or scCtrl,
    VK_F4 or scShift or scCtrl,
    VK_F5 or scShift or scCtrl,
    VK_F6 or scShift or scCtrl,
    VK_F7 or scShift or scCtrl,
    VK_F8 or scShift or scCtrl,
    VK_F9 or scShift or scCtrl,
    VK_F10 or scShift or scCtrl,
    VK_F11 or scShift or scCtrl,
    VK_F12 or scShift or scCtrl,
    VK_INSERT,
    VK_INSERT or scShift,
    VK_INSERT or scCtrl,
    VK_DELETE,
    VK_DELETE or scShift,
    VK_DELETE or scCtrl,
    VK_BACK or scAlt,
    VK_BACK or scShift or scAlt);

function TShortCutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

function TShortCutProperty.GetValue: string;
var
  CurValue: TShortCut;
begin
  CurValue := GetOrdValue;
  if CurValue = scNone then
    Result := srNone else
    Result := ShortCutToText(CurValue);
end;

procedure TShortCutProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  Proc(srNone);
  for I := 1 to High(ShortCuts) do Proc(ShortCutToText(ShortCuts[I]));
end;

procedure TShortCutProperty.SetValue(const Value: string);
var
  NewValue: TShortCut;
begin
  NewValue := 0;
  if (Value <> '') and (AnsiCompareText(Value, srNone) <> 0) then
  begin
    NewValue := TextToShortCut(Value);
    if NewValue = 0 then
      raise EPropertyError.CreateRes(@SInvalidPropertyValue);
  end;
  SetOrdValue(NewValue);
end;

{ TTabOrderProperty }

function TTabOrderProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [];
end;

{ TCaptionProperty }

function TCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paAutoUpdate, paRevertable];
end;

{ TDateProperty }

function TDateProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
end;

function TDateProperty.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = 0.0 then Result := '' else
  Result := DateToStr(DT);
end;

procedure TDateProperty.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then DT := 0.0
  else DT := StrToDate(Value);
  SetFloatValue(DT);
end;

{ TTimeProperty }

function TTimeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
end;

function TTimeProperty.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = 0.0 then Result := '' else
  Result := TimeToStr(DT);
end;

procedure TTimeProperty.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then DT := 0.0
  else DT := StrToTime(Value);
  SetFloatValue(DT);
end;

function TDateTimeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
end;

function TDateTimeProperty.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = 0.0 then Result := '' else
  Result := DateTimeToStr(DT);
end;

procedure TDateTimeProperty.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then DT := 0.0
  else DT := StrToDateTime(Value);
  SetFloatValue(DT);
end;

{ TPropInfoList }

type
  TPropInfoList = class
  private
    FList: PPropList;
    FCount: Integer;
    FSize: Integer;
    function Get(Index: Integer): PPropInfo;
  public
    constructor Create(Instance: TPersistent; Filter: TTypeKinds);
    destructor Destroy; override;
    function Contains(P: PPropInfo): Boolean;
    procedure Delete(Index: Integer);
    procedure Intersect(List: TPropInfoList);
    property Count: Integer read FCount;
    property Items[Index: Integer]: PPropInfo read Get; default;
  end;

constructor TPropInfoList.Create(Instance: TPersistent; Filter: TTypeKinds);
begin
  FCount := GetPropList(Instance.ClassInfo, Filter, nil);
  FSize := FCount * SizeOf(Pointer);
  GetMem(FList, FSize);
  GetPropList(Instance.ClassInfo, Filter, FList);
end;

destructor TPropInfoList.Destroy;
begin
  if FList <> nil then FreeMem(FList, FSize);
end;

function TPropInfoList.Contains(P: PPropInfo): Boolean;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    with FList^[I]^ do
      if (PropType^ = P^.PropType^) and (CompareText(Name, P^.Name) = 0) then
      begin
        Result := True;
        Exit;
      end;
  Result := False;
end;

procedure TPropInfoList.Delete(Index: Integer);
begin
  Dec(FCount);
  if Index < FCount then
    Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Pointer));
end;

function TPropInfoList.Get(Index: Integer): PPropInfo;
begin
  Result := FList^[Index];
end;

procedure TPropInfoList.Intersect(List: TPropInfoList);
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if not List.Contains(FList^[I]) then Delete(I);
end;

{ GetComponentProperties }

procedure RegisterPropertyEditor(PropertyType: PTypeInfo; ComponentClass: TClass;
  const PropertyName: string; EditorClass: TPropertyEditorClass);
var
  P: PPropertyClassRec;
begin
  if PropertyClassList = nil then
    PropertyClassList := TList.Create;
  New(P);
  P.Group := CurrentGroup;
  P.PropertyType := PropertyType;
  P.ComponentClass := ComponentClass;
  P.PropertyName := '';
  if Assigned(ComponentClass) then P^.PropertyName := PropertyName;
  P.EditorClass := EditorClass;
  PropertyClassList.Insert(0, P);
end;

procedure RegisterPropertyMapper(Mapper: TPropertyMapperFunc);
var
  P: PPropertyMapperRec;
begin
  if PropertyMapperList = nil then
    PropertyMapperList := TList.Create;
  New(P);
  P^.Group := CurrentGroup;
  P^.Mapper := Mapper;
  PropertyMapperList.Insert(0, P);
end;

function GetEditorClass(PropInfo: PPropInfo;
  Obj: TPersistent): TPropertyEditorClass;
var
  PropType: PTypeInfo;
  P, C: PPropertyClassRec;
  I: Integer;
begin
  if PropertyMapperList <> nil then
  begin
    for I := 0 to PropertyMapperList.Count -1 do
      with PPropertyMapperRec(PropertyMapperList[I])^ do
      begin
        Result := Mapper(Obj, PropInfo);
        if Result <> nil then Exit;
      end;
  end;
  PropType := PropInfo^.PropType^;
  I := 0;
  C := nil;
  while I < PropertyClassList.Count do
  begin
    P := PropertyClassList[I];

    if ((P^.PropertyType = PropType) or
         ((P^.PropertyType^.Kind = PropType.Kind) and
          (P^.PropertyType^.Name = PropType.Name)
         )
       ) or
       ( (PropType^.Kind = tkClass) and
         (P^.PropertyType^.Kind = tkClass) and
         GetTypeData(PropType)^.ClassType.InheritsFrom(GetTypeData(P^.PropertyType)^.ClassType)
       ) then
      if ((P^.ComponentClass = nil) or (Obj.InheritsFrom(P^.ComponentClass))) and
         ((P^.PropertyName = '') or (CompareText(PropInfo^.Name, P^.PropertyName) = 0)) then
        if (C = nil) or   // see if P is better match than C
           ((C^.ComponentClass = nil) and (P^.ComponentClass <> nil)) or
           ((C^.PropertyName = '') and (P^.PropertyName <> ''))
           or  // P's proptype match is exact, but C's isn't
           ((C^.PropertyType <> PropType) and (P^.PropertyType = PropType))
           or  // P's proptype is more specific than C's proptype
           ((P^.PropertyType <> C^.PropertyType) and
            (P^.PropertyType^.Kind = tkClass) and
            (C^.PropertyType^.Kind = tkClass) and
            GetTypeData(P^.PropertyType)^.ClassType.InheritsFrom(
              GetTypeData(C^.PropertyType)^.ClassType))
           or // P's component class is more specific than C's component class
           ((P^.ComponentClass <> nil) and (C^.ComponentClass <> nil) and
            (P^.ComponentClass <> C^.ComponentClass) and
            (P^.ComponentClass.InheritsFrom(C^.ComponentClass))) then
          C := P;
    Inc(I);
  end;
  if C <> nil then
    Result := C^.EditorClass else
    Result := PropClassMap[PropType^.Kind];
end;

procedure GetComponentProperties(Components: TDesignerSelectionList;
  Filter: TTypeKinds; Designer: IFormDesigner; Proc: TGetPropEditProc);
var
  I, J, CompCount: Integer;
  CompType: TClass;
  Candidates: TPropInfoList;
  PropLists: TList;
  Editor: TPropertyEditor;
  EdClass: TPropertyEditorClass;
  PropInfo: PPropInfo;
  AddEditor: Boolean;
  Obj: TPersistent;
begin
  if (Components = nil) or (Components.Count = 0) then Exit;
  CompCount := Components.Count;
  Obj := Components[0];
  CompType := Components[0].ClassType;
  Candidates := TPropInfoList.Create(Components[0], Filter);
  try
    for I := Candidates.Count - 1 downto 0 do
    begin
      PropInfo := Candidates[I];
      EdClass := GetEditorClass(PropInfo, Obj);
      if EdClass = nil then
        Candidates.Delete(I)
      else
      begin
        Editor := EdClass.Create(Designer, 1);
        try
          Editor.SetPropEntry(0, Components[0], PropInfo);
          Editor.Initialize;
          with PropInfo^ do
            if (GetProc = nil) or ((PropType^.Kind <> tkClass) and
              (SetProc = nil)) or ((CompCount > 1) and
              not (paMultiSelect in Editor.GetAttributes)) or
              not Editor.ValueAvailable then
              Candidates.Delete(I);
        finally
          Editor.Free;
        end;
      end;
    end;
    PropLists := TList.Create;
    try
      PropLists.Capacity := CompCount;
      for I := 0 to CompCount - 1 do
        PropLists.Add(TPropInfoList.Create(Components[I], Filter));
      for I := 0 to CompCount - 1 do
        Candidates.Intersect(TPropInfoList(PropLists[I]));
      for I := 0 to CompCount - 1 do
        TPropInfoList(PropLists[I]).Intersect(Candidates);
      for I := 0 to Candidates.Count - 1 do
      begin
        EdClass := GetEditorClass(Candidates[I], Obj);
        if EdClass = nil then Continue;
        Editor := EdClass.Create(Designer, CompCount);
        try
          AddEditor := True;
          for J := 0 to CompCount - 1 do
          begin
            if (Components[J].ClassType <> CompType) and
              (GetEditorClass(TPropInfoList(PropLists[J])[I],
                Components[J]) <> Editor.ClassType) then
            begin
              AddEditor := False;
              Break;
            end;
            Editor.SetPropEntry(J, Components[J],
              TPropInfoList(PropLists[J])[I]);
          end;
        except
          Editor.Free;
          raise;
        end;
        if AddEditor then
        begin
          Editor.Initialize;
          if Editor.ValueAvailable then
            Proc(Editor) else
            Editor.Free;
        end
        else Editor.Free;
      end;
    finally
      for I := 0 to PropLists.Count - 1 do TPropInfoList(PropLists[I]).Free;
      PropLists.Free;
    end;
  finally
    Candidates.Free;
  end;
end;

{ RegisterComponentEditor }

type
  PComponentClassRec = ^TComponentClassRec;
  TComponentClassRec = record
    Group: Integer;
    ComponentClass: TComponentClass;
    EditorClass: TComponentEditorClass;
  end;

var
  ComponentClassList: TList = nil;

procedure RegisterComponentEditor(ComponentClass: TComponentClass;
  ComponentEditor: TComponentEditorClass);
var
  P: PComponentClassRec;
begin
  if ComponentClassList = nil then
    ComponentClassList := TList.Create;
  New(P);
  P.Group := CurrentGroup;
  P.ComponentClass := ComponentClass;
  P.EditorClass := ComponentEditor;
  ComponentClassList.Insert(0, P);
end;

{ GetComponentEditor }

function GetComponentEditor(Component: TComponent;
  Designer: IFormDesigner): TComponentEditor;
var
  P: PComponentClassRec;
  I: Integer;
  ComponentClass: TComponentClass;
  EditorClass: TComponentEditorClass;
begin
  ComponentClass := TComponentClass(TPersistent);
  EditorClass := TDefaultEditor;
  for I := 0 to ComponentClassList.Count-1 do
  begin
    P := ComponentClassList[I];
    if (Component is P^.ComponentClass) and
      (P^.ComponentClass <> ComponentClass) and
      (P^.ComponentClass.InheritsFrom(ComponentClass)) then
    begin
      EditorClass := P^.EditorClass;
      ComponentClass := P^.ComponentClass;
    end;
  end;
  Result := EditorClass.Create(Component, Designer);
end;

{ package management }

var
  FGroupNotifyList: TList = nil;

function NewEditorGroup: Integer;
begin
  if EditorGroupList = nil then
    EditorGroupList := TBits.Create;
  CurrentGroup := EditorGroupList.OpenBit;
  EditorGroupList[CurrentGroup] := True;
  Result := CurrentGroup;
end;

procedure NotifyGroupChange(AProc: TGroupChangeProc);
begin
  UnNotifyGroupChange(AProc);
  if not Assigned(FGroupNotifyList) then
    FGroupNotifyList := TList.Create;
  FGroupNotifyList.Add(@AProc);
end;

procedure UnNotifyGroupChange(AProc: TGroupChangeProc);
begin
  if Assigned(FGroupNotifyList) then
    FGroupNotifyList.Remove(@AProc);
end;

procedure FreeEditorGroup(Group: Integer);
var
  I: Integer;
  P: PPropertyClassRec;
  C: PComponentClassRec;
  M: PPropertyMapperRec;
begin
  I := PropertyClassList.Count - 1;
  while I > -1 do
  begin
    P := PropertyClassList[I];
    if P.Group = Group then
    begin
      PropertyClassList.Delete(I);
      Dispose(P);
    end;
    Dec(I);
  end;
  I := ComponentClassList.Count - 1;
  while I > -1 do
  begin
    C := ComponentClassList[I];
    if C.Group = Group then
    begin
      ComponentClassList.Delete(I);
      Dispose(C);
    end;
    Dec(I);
  end;
  if PropertyMapperList <> nil then
    for I := PropertyMapperList.Count-1 downto 0 do
    begin
      M := PropertyMapperList[I];
      if M.Group = Group then
      begin
        PropertyMapperList.Delete(I);
        Dispose(M);
      end;
    end;
  if InternalPropertyCategoryList <> nil then
    InternalPropertyCategoryList.FreeEditorGroup(Group);
  if Assigned(FreeCustomModulesProc) then
    FreeCustomModulesProc(Group);
  if Assigned(FGroupNotifyList) then
    for I := FGroupNotifyList.Count - 1 downto 0 do
      TGroupChangeProc(FGroupNotifyList[I])(Group);
  if (Group >= 0) and (Group < EditorGroupList.Size) then
    EditorGroupList[Group] := False;
end;

{ TComponentEditor }

constructor TComponentEditor.Create(AComponent: TComponent; ADesigner: IFormDesigner);
begin
  inherited Create;
  FComponent := AComponent;
  FDesigner := ADesigner;
end;

procedure TComponentEditor.Edit;
begin
  if GetVerbCount > 0 then ExecuteVerb(0);
end;

function TComponentEditor.GetIComponent: IComponent;
begin
  Result := MakeIComponent(FComponent);
end;

function TComponentEditor.GetDesigner: IFormDesigner;
begin
  Result := FDesigner;
end;

function TComponentEditor.GetVerbCount: Integer;
begin
  Result := 0;
end;

function TComponentEditor.GetVerb(Index: Integer): string;
begin
end;

procedure TComponentEditor.ExecuteVerb(Index: Integer);
begin
end;

procedure TComponentEditor.Copy;
begin
end;

function TComponentEditor.IsInInlined: Boolean;
begin
  Result := csInline in Component.Owner.ComponentState;
end;

procedure TComponentEditor.PrepareItem(Index: Integer; const AItem: TMenuItem);
begin
end;

{ TDefaultEditor }

procedure TDefaultEditor.CheckEdit(PropertyEditor: TPropertyEditor);
var
  FreeEditor: Boolean;
begin
  FreeEditor := True;
  try
    if FContinue then EditProperty(PropertyEditor, FContinue, FreeEditor);
  finally
    if FreeEditor then PropertyEditor.Free;
  end;
end;

procedure TDefaultEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
var
  PropName: string;
  BestName: string;

  procedure ReplaceBest;
  begin
    FBest.Free;
    FBest := PropertyEditor;
    if FFirst = FBest then FFirst := nil;
    FreeEditor := False;
  end;

begin
  if not Assigned(FFirst) and (PropertyEditor is TMethodProperty) then
  begin
    FreeEditor := False;
    FFirst := PropertyEditor;
  end;
  PropName := PropertyEditor.GetName;
  BestName := '';
  if Assigned(FBest) then BestName := FBest.GetName;
  if CompareText(PropName, 'ONCREATE') = 0 then
    ReplaceBest
  else if CompareText(BestName, 'ONCREATE') <> 0 then
    if CompareText(PropName, 'ONCHANGE') = 0 then
      ReplaceBest
    else if CompareText(BestName, 'ONCHANGE') <> 0 then
      if CompareText(PropName, 'ONCLICK') = 0 then
        ReplaceBest;
end;

procedure TDefaultEditor.Edit;
var
  Components: TDesignerSelectionList;
begin
  Components := TDesignerSelectionList.Create;
  try
    FContinue := True;
    Components.Add(Component);
    FFirst := nil;
    FBest := nil;
    try
      GetComponentProperties(Components, tkAny, Designer, CheckEdit);
      if FContinue then
        if Assigned(FBest) then
          FBest.Edit
        else if Assigned(FFirst) then
          FFirst.Edit;
    finally
      FFirst.Free;
      FBest.Free;
    end;
  finally
    Components.Free;
  end;
end;

{ TCustomModule }

constructor TCustomModule.Create(ARoot: IComponent);
begin
  inherited Create;
  FRoot := ARoot;
end;

procedure TCustomModule.ExecuteVerb(Index: Integer);
begin
end;

function TCustomModule.CreateDesignerForm(Designer: IDesigner): TCustomForm;
begin
  Result := nil;
end;

function TCustomModule.GetAttributes: TCustomModuleAttributes;
begin
  Result := [];
end;

function TCustomModule.GetVerb(Index: Integer): string;
begin
  Result := '';
end;

function TCustomModule.GetVerbCount: Integer;
begin
  Result := 0;
end;

procedure TCustomModule.Saving;
begin
end;

procedure TCustomModule.ValidateComponent(Component: IComponent);
begin
end;

procedure RegisterCustomModule(ComponentBaseClass: TComponentClass;
  CustomModuleClass: TCustomModuleClass);
begin
  if Assigned(RegisterCustomModuleProc) then
    RegisterCustomModuleProc(CurrentGroup, ComponentBaseClass, CustomModuleClass);
end;

function MakeIPersistent(Instance: TPersistent): IPersistent;
begin
  if Assigned(MakeIPersistentProc) then
    Result := MakeIPersistentProc(Instance);
end;

function ExtractPersistent(const Intf: IUnknown): TPersistent;
begin
  if Intf = nil then
    Result := nil
  else
    Result := (Intf as IImplementation).GetInstance as TPersistent;
end;

function TryExtractPersistent(const Intf: IUnknown): TPersistent;
var
  Temp: IImplementation;
begin
  Result := nil;
  if (Intf <> nil) and (Intf.QueryInterface(IImplementation, Temp) = 0) and
    (Temp.GetInstance <> nil) and (Temp.GetInstance is TPersistent) then
    Result := TPersistent(Temp.GetInstance);
end;

function MakeIComponent(Instance: TComponent): IComponent;
begin
  if Assigned(MakeIComponentProc) then
    Result := MakeIComponentProc(Instance);
end;

function ExtractComponent(const Intf: IUnknown): TComponent;
begin
  if Intf = nil then
    Result := nil
  else
    Result := (Intf as IImplementation).GetInstance as TComponent;
end;

function TryExtractComponent(const Intf: IUnknown): TComponent;
var
  Temp: TPersistent;
begin
  Temp := TryExtractPersistent(Intf);
  if (Temp <> nil) and (Temp is TComponent) then
    Result := TComponent(Temp)
  else
    Result := nil;
end;

type

  { TSelectionList  -  implements IDesignerSelections }

  TSelectionList = class(TInterfacedObject, IDesignerSelections)
  private
    FList: IInterfaceList;
  public
    constructor Create;
    function Add(const Item: IPersistent): Integer;
    function Equals(const List: IDesignerSelections): Boolean;
    function Get(Index: Integer): IPersistent;
    function GetCount: Integer;
  end;

constructor TSelectionList.Create;
begin
  inherited Create;
  FList := TInterfaceList.Create;
end;

function TSelectionList.Add(const Item: IPersistent): Integer;
begin
  Result := FList.Add(Item);
end;

function TSelectionList.Equals(const List: IDesignerSelections): Boolean;
var
  I: Integer;
begin
  Result := False;
  if List.Count <> FList.Count then Exit;
  for I := 0 to List.Count - 1 do if not List[I].Equals(IPersistent(FList[I])) then Exit;
  Result := True;
end;

function TSelectionList.Get(Index: Integer): IPersistent;
begin
  Result := IPersistent(FList[Index]);
end;

function TSelectionList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function CreateSelectionList: IDesignerSelections;
begin
  Result := TDesignerSelectionList.Create;
end;

class function TCustomModule.Nestable: Boolean;
begin
  Result := False;
end;

procedure TCustomModule.PrepareItem(Index: Integer; const AItem: TMenuItem);
begin
end;

{ TPropertyFilter }

constructor TPropertyFilter.Create(const APropertyName: String;
  AComponentClass: TClass; APropertyType: PTypeInfo);
begin
  inherited Create;
  if APropertyName <> '' then
    FMask := TMask.Create(APropertyName);
  FComponentClass := AComponentClass;
  FPropertyType := APropertyType;
  FGroup := CurrentGroup;
end;

destructor TPropertyFilter.Destroy;
begin
  FMask.Free;
  inherited Destroy;
end;

function TPropertyFilter.Match(const APropertyName: String;
  AComponentClass: TClass; APropertyType: PTypeInfo): Boolean;
  function MatchName: Boolean;
  begin
    Result := not Assigned(FMask) or
              FMask.Matches(APropertyName);
  end;
  function MatchClass: Boolean;
  begin
    Result := Assigned(AComponentClass) and
              ((ComponentClass = AComponentClass) or
               (AComponentClass.InheritsFrom(ComponentClass)));
  end;
  function MatchType: Boolean;
  begin
    Result := Assigned(APropertyType) and
              ((PropertyType = APropertyType) or
               ((PropertyType^.Kind = tkClass) and
                (APropertyType^.Kind = tkClass) and
                GetTypeData(APropertyType)^.ClassType.InheritsFrom(GetTypeData(PropertyType)^.ClassType)));
  end;
begin
  if Assigned(ComponentClass) then
    if Assigned(PropertyType) then
      Result := MatchClass and MatchType and MatchName
    else
      Result := MatchClass and MatchName
  else
    if Assigned(PropertyType) then
      Result := MatchType and MatchName
    else
      Result := MatchName;
end;

{ TPropertyCategory }

function TPropertyCategory.Add(AFilter: TPropertyFilter): TPropertyFilter;
begin
  FList.Insert(0, AFilter);
  Result := AFilter;
end;

procedure TPropertyCategory.ClearMatches;
begin
  FMatchCount := 0;
end;

function TPropertyCategory.Count: integer;
begin
  Result := FList.Count;
end;

constructor TPropertyCategory.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
  FVisible := True;
  FEnabled := True;
  FGroup := CurrentGroup;
end;

class function TPropertyCategory.Description: string;
begin
  Result := Name;
end;

destructor TPropertyCategory.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TPropertyCategory.FreeEditorGroup(AGroup: Integer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Filters[I].FGroup = AGroup then
      FList.Delete(I);
end;

function TPropertyCategory.GetFilter(Index: Integer): TPropertyFilter;
begin
  Result := TPropertyFilter(FList[Index])
end;

function TPropertyCategory.Match(const APropertyName: String;
  AComponentClass: TClass; APropertyType: PTypeInfo): Boolean;
var
  I: Integer;
  vPropInfo: PPropInfo;
begin
  Result := False;

  if not Assigned(APropertyType) and
     Assigned(AComponentClass) then
  begin
    vPropInfo := GetPropInfo(PTypeInfo(AComponentClass.ClassInfo), APropertyName);
    if Assigned(vPropInfo) then
      APropertyType := vPropInfo.PropType^;
  end;

  for I := 0 to Count - 1 do
    Result := Result or
              Filters[I].Match(APropertyName, AComponentClass, APropertyType);
  if Result then
    Inc(FMatchCount);
end;

class function TPropertyCategory.Name: string;
begin
  Result := '';
  raise EPropertyError.CreateRes(@SInvalidCategory);
end;

procedure TPropertyCategory.PropDraw(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  with ACanvas do
    TextRect(ARect, ARect.Left + 1, ARect.Top + 1, Name);
end;

{ TPropertyCategoryList }

function TPropertyCategoryList.ChangeVisibility(AMode: TPropertyCategoryVisibleMode): Boolean;
begin
  Result := ChangeVisibility(AMode, [nil]);
end;

function TPropertyCategoryList.ChangeVisibility(AMode: TPropertyCategoryVisibleMode;
  const AClasses: array of TClass): Boolean;
var
  I: Integer;
  vChanged: Boolean;
  procedure ChangeIfNot(ACategory: TPropertyCategory; Value: Boolean);
  begin
    if ACategory.Visible <> Value then
    begin
      ACategory.Visible := Value;
      vChanged := True;
    end;
  end;
  function ListedCategory(AClass: TClass): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if AClasses[Low(AClasses)] <> nil then
      for I := Low(AClasses) to High(AClasses) do
        if AClass = AClasses[I] then
        begin
          Result := True;
          break;
        end;
  end;
begin
  vChanged := False;
  for I := 0 to Count - 1 do
    case AMode of
      pcvAll:        ChangeIfNot(Categories[I], True);
      pcvToggle:     ChangeIfNot(Categories[I], not Categories[I].Visible);
      pcvNone:       ChangeIfNot(Categories[I], False);
      pcvNotListed:  ChangeIfNot(Categories[I], not ListedCategory(Categories[I].ClassType));
      pcvOnlyListed: ChangeIfNot(Categories[I], ListedCategory(Categories[I].ClassType));
    end;
  Result := vChanged;
end;

procedure TPropertyCategoryList.ClearMatches;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Categories[I].ClearMatches;
end;

function TPropertyCategoryList.Count: integer;
begin
  Result := FList.Count
end;

constructor TPropertyCategoryList.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
end;

destructor TPropertyCategoryList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TPropertyCategoryList.FindCategory(ACategoryClass: TPropertyCategoryClass): TPropertyCategory;
var
  I: Integer;
begin
  I := IndexOf(ACategoryClass);
  if I <> -1 then
    Result := Categories[I]
  else
  begin
    Result := ACategoryClass.Create;
    FList.Insert(0, Result);
  end;
end;

procedure TPropertyCategoryList.FreeEditorGroup(AGroup: Integer);
var
  I: Integer;
begin
  FMiscCategory := nil;
  for I := Count - 1 downto 0 do
    if Categories[I].FGroup = AGroup then
      FList.Delete(I)
    else
      Categories[I].FreeEditorGroup(AGroup);
end;

function TPropertyCategoryList.GetCategory(Index: Integer): TPropertyCategory;
begin
  Result := TPropertyCategory(FList[Index])
end;

function TPropertyCategoryList.GetHiddenCategories: string;
var
  vStrings: TStringList;
  I: Integer;
begin
  vStrings := TStringList.Create;
  try
    for I := 0 to Count - 1 do
      if not Categories[I].Visible then
        vStrings.Add(Categories[I].Name);
  finally
    Result := vStrings.CommaText;
    vStrings.Free;
  end;
end;

function TPropertyCategoryList.IndexOf(const ACategoryName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Categories[I].Name = ACategoryName then
    begin
      Result := I;
      break;
    end;
end;

function TPropertyCategoryList.IndexOf(ACategoryClass: TPropertyCategoryClass): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Categories[I].ClassType = ACategoryClass then
    begin
      Result := I;
      break;
    end;
end;

function TPropertyCategoryList.Match(const APropertyName: String;
  AComponentClass: TClass; APropertyType: PTypeInfo = nil): Boolean;
var
  I: Integer;
  vThisMatch, vAnyMatches: Boolean;
  vPropInfo: PPropInfo;
begin
  // assume the worst
  Result := False;
  vAnyMatches := False;

  // make sure we have good data
  if not Assigned(APropertyType) and
     Assigned(AComponentClass) then
  begin
    vPropInfo := GetPropInfo(PTypeInfo(AComponentClass.ClassInfo), APropertyName);
    if Assigned(vPropInfo) then
      APropertyType := vPropInfo.PropType^;
  end;

  // for each category...
  for I := 0 to Count - 1 do
    if Categories[I] <> MiscCategory then begin

      // found something?
      vThisMatch := Categories[I].Match(APropertyName, AComponentClass, APropertyType);
      vAnyMatches := vAnyMatches or vThisMatch;

      // if this is a good match and its visible then...
      Result := vThisMatch and Categories[I].Visible;
      if Result then
        break;
    end;

  // if no matches then check the misc category
  if not vAnyMatches then
  begin
    vThisMatch := MiscCategory.Match(APropertyName, AComponentClass, APropertyType);
    Result := vThisMatch and MiscCategory.Visible;
  end;
end;

function TPropertyCategoryList.MiscCategory: TPropertyCategory;
begin
  if FMiscCategory = nil then
    FMiscCategory := FindCategory(TMiscellaneousCategory);
  Result := FMiscCategory;
end;

procedure TPropertyCategoryList.SetHiddenCategories(const Value: string);
var
  vStrings: TStringList;
  I: Integer;
begin
  vStrings := TStringList.Create;
  try
    vStrings.CommaText := Value;
    for I := 0 to Count - 1 do
      Categories[I].Visible := vStrings.IndexOf(Categories[I].Name) = -1;
  finally
    vStrings.Free;
  end;
end;

{ Property Categories }

function RegisterPropertyInCategory(ACategoryClass: TPropertyCategoryClass;
  const APropertyName: string): TPropertyFilter;
begin
  Result := PropertyCategoryList.FindCategory(ACategoryClass).Add(
    TPropertyFilter.Create(APropertyName, nil, nil));
end;

function RegisterPropertyInCategory(ACategoryClass: TPropertyCategoryClass;
  AComponentClass: TClass; const APropertyName: string): TPropertyFilter; overload;
begin
  Result := PropertyCategoryList.FindCategory(ACategoryClass).Add(
    TPropertyFilter.Create(APropertyName, AComponentClass, nil));
end;

function RegisterPropertyInCategory(ACategoryClass: TPropertyCategoryClass;
  APropertyType: PTypeInfo; const APropertyName: string): TPropertyFilter; overload;
begin
  Result := PropertyCategoryList.FindCategory(ACategoryClass).Add(
    TPropertyFilter.Create(APropertyName, nil, APropertyType));
end;

function RegisterPropertyInCategory(ACategoryClass: TPropertyCategoryClass;
  APropertyType: PTypeInfo): TPropertyFilter;
begin
  Result := PropertyCategoryList.FindCategory(ACategoryClass).Add(
    TPropertyFilter.Create('', nil, APropertyType));
end;

function RegisterPropertiesInCategory(ACategoryClass: TPropertyCategoryClass;
  const AFilters: array of const): TPropertyCategory;
var
  I: Integer;
begin
  Result := PropertyCategoryList.FindCategory(ACategoryClass);
  for I := Low(AFilters) to High(AFilters) do
    with AFilters[I], Result do
      case vType of
        vtPointer:    Add(TPropertyFilter.Create('', nil, PTypeInfo(vPointer)));
        vtClass:      Add(TPropertyFilter.Create('', vClass, nil));
        vtAnsiString: Add(TPropertyFilter.Create(String(vAnsiString), nil, nil));
      else
        raise EPropertyError.CreateResFmt(@SInvalidFilter, [I, vType]);
      end;
end;

function RegisterPropertiesInCategory(ACategoryClass: TPropertyCategoryClass;
  AComponentClass: TClass; const AFilters: array of string): TPropertyCategory;
var
  I: Integer;
begin
  Result := PropertyCategoryList.FindCategory(ACategoryClass);
  for I := Low(AFilters) to High(AFilters) do
    Result.Add(TPropertyFilter.Create(AFilters[I], AComponentClass, nil));
end;

function RegisterPropertiesInCategory(ACategoryClass: TPropertyCategoryClass;
  APropertyType: PTypeInfo; const AFilters: array of string): TPropertyCategory;
var
  I: Integer;
begin
  Result := PropertyCategoryList.FindCategory(ACategoryClass);
  for I := Low(AFilters) to High(AFilters) do
    Result.Add(TPropertyFilter.Create(AFilters[I], nil, APropertyType));
end;

function IsPropertyInCategory(ACategoryClass: TPropertyCategoryClass;
  AComponentClass: TClass; const APropertyName: String): Boolean;
begin
  Result := PropertyCategoryList.FindCategory(ACategoryClass).Match(
    APropertyName, AComponentClass, nil);
end;

function IsPropertyInCategory(ACategoryClass: TPropertyCategoryClass;
  const AClassName: string; const APropertyName: String): Boolean;
begin
  Result := PropertyCategoryList.FindCategory(ACategoryClass).Match(
    APropertyName, FindClass(AClassName), nil);
end;

function PropertyCategoryList: TPropertyCategoryList;
begin
  // if it doesn't exists then make it
  if not Assigned(InternalPropertyCategoryList) then
  begin
    InternalPropertyCategoryList := TPropertyCategoryList.Create;

    // add the catch all misc category
    InternalPropertyCategoryList.FindCategory(TMiscellaneousCategory).Add(
      TPropertyFilter.Create('', nil, nil));
  end;

  // ok return it then
  Result := InternalPropertyCategoryList;
end;

{ TActionCategory }

class function TActionCategory.Name: string;
begin
  Result := SActionCategoryName;
end;

class function TActionCategory.Description: string;
begin
  Result := SActionCategoryDesc;
end;

{ TDataCategory }

class function TDataCategory.Name: string;
begin
  Result := SDataCategoryName;
end;

class function TDataCategory.Description: string;
begin
  Result := SDataCategoryDesc;
end;

{ TDatabaseCategory }

class function TDatabaseCategory.Name: string;
begin
  Result := SDatabaseCategoryName;
end;

class function TDatabaseCategory.Description: string;
begin
  Result := SDatabaseCategoryDesc;
end;

{ TDragNDropCategory }

class function TDragNDropCategory.Name: string;
begin
  Result := SDragNDropCategoryName;
end;

class function TDragNDropCategory.Description: string;
begin
  Result := SDragNDropCategoryDesc;
end;

{ THelpCategory }

class function THelpCategory.Name: string;
begin
  Result := SHelpCategoryName;
end;

class function THelpCategory.Description: string;
begin
  Result := SHelpCategoryDesc;
end;

{ TLayoutCategory }

class function TLayoutCategory.Name: string;
begin
  Result := SLayoutCategoryName;
end;

class function TLayoutCategory.Description: string;
begin
  Result := SLayoutCategoryDesc;
end;

{ TLegacyCategory }

class function TLegacyCategory.Name: string;
begin
  Result := SLegacyCategoryName;
end;

class function TLegacyCategory.Description: string;
begin
  Result := SLegacyCategoryDesc;
end;

{ TLinkageCategory }

class function TLinkageCategory.Name: string;
begin
  Result := SLinkageCategoryName;
end;

class function TLinkageCategory.Description: string;
begin
  Result := SLinkageCategoryDesc;
end;

{ TLocaleCategory }

class function TLocaleCategory.Name: string;
begin
  Result := SLocaleCategoryName;
end;

class function TLocaleCategory.Description: string;
begin
  Result := SLocaleCategoryDesc;
end;

{ TLocalizableCategory }

class function TLocalizableCategory.Name: string;
begin
  Result := SLocalizableCategoryName;
end;

class function TLocalizableCategory.Description: string;
begin
  Result := SLocalizableCategoryDesc;
end;

{ TMiscellaneousCategory }

class function TMiscellaneousCategory.Name: string;
begin
  Result := SMiscellaneousCategoryName;
end;

class function TMiscellaneousCategory.Description: string;
begin
  Result := SMiscellaneousCategoryDesc;
end;

{ TVisualCategory }

class function TVisualCategory.Name: string;
begin
  Result := SVisualCategoryName;
end;

class function TVisualCategory.Description: string;
begin
  Result := SVisualCategoryDesc;
end;

{ TInputCategory }

class function TInputCategory.Name: string;
begin
  Result := SInputCategoryName;
end;

class function TInputCategory.Description: string;
begin
  Result := SInputCategoryDesc;
end;

initialization

finalization
  FreeAndNil(EditorGroupList);
  FreeAndNil(PropertyClassList);
  FreeAndNil(ComponentClassList);
  FreeAndNil(PropertyMapperList);
  FreeAndNil(InternalPropertyCategoryList);
end.

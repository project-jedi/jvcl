{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRegAutoEditorForm.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Design-time Editor for TJvRegAuto component

Known Issues:
  Some russian comments were translated to english; these comments are marked
  with [translated]
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvRegAutoEditorForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, Menus,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  {$IFDEF COMPILER4_UP}
  ImgList,
  {$ENDIF COMPILER4_UP}
  JvRegAuto, JvComponent;

type
  TJvRegAutoEditor = class(TComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TJvRegEditor = class(TForm)
    panelBottom: TPanel;
    panelTop: TPanel;
    panelOKCancelApply: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    panelButtons: TPanel;
    edtProp: TEdit;
    TreeImages: TImageList;
    Tree: TTreeView;
    btnAddProp: TSpeedButton;
    List: TListBox;
    PopupMenu1: TPopupMenu;
    Sort1: TMenuItem;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure btnAddPropClick(Sender: TObject);
    procedure ListKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListClick(Sender: TObject);
    procedure TreeEnter(Sender: TObject);
    procedure ListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure edtPropChange(Sender: TObject);
    procedure edtPropKeyPress(Sender: TObject; var Key: Char);
    procedure ListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListEnter(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Sort1Click(Sender: TObject);
  private
    Component: TJvRegAuto;
    FProps: string;
    FModified: Boolean;
    procedure Apply;
    procedure TreeLoad;
    procedure ListLoad;
    procedure PropAdd;
    procedure PropDelete;
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
  public
    constructor Create1(AOwner: TComponent; lComponent: TComponent);
    property Modified: Boolean read FModified;
  end;

implementation

uses
  JvConsts, JvCtlConst, TypInfo, ExptIntf, JvDsgnIntf;

{$R *.DFM}

//=== TJvRegAutoEditor =======================================================

function TJvRegAutoEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TJvRegAutoEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1 then
    Result := 'Editor'
  else
    Result := inherited GetVerb(Index);
end;

procedure TJvRegAutoEditor.ExecuteVerb(Index: Integer);
var
  RegEditor: TJvRegEditor;
begin
  if Index = GetVerbCount - 1 then
  begin
    RegEditor := TJvRegEditor.Create1(nil, Component);
    try
      RegEditor.ShowModal;
      if RegEditor.Modified then
        Designer.Modified;
    finally
      RegEditor.Free;
    end
  end
  else
    inherited ExecuteVerb(Index);
end;

//=== TJvLoadProgress ========================================================

type
  TJvLoadProgress = class(TForm)
    ProgressBar: TProgressBar;
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    Canceled: Boolean;
  end;

var
  LoadProgress: TJvLoadProgress;

const
  // Indexes of pictures in TreeImages [translated]
  imUnknown = 4;
  imClass = 4;
  imInteger = 1;
  imChar = 1;
  imString = 1;
  imEnumeration = 1;
  imComponent = 3; // With this picture start komponety [translated]

function LoadProgressCreate: TJvLoadProgress;
begin
  {$IFDEF DELPHI}
  Result := TJvLoadProgress.CreateNew(Application);
  {$ELSE}
  Result := TJvLoadProgress.CreateNew(Application, 1);
  {$ENDIF}
  with Result do
  begin
    OnClose := FormClose;
    Width := 279;
    Height := 148;
    BorderStyle := bsDialog;
    Position := poScreenCenter;
    Caption := 'JvRegAuto Editor';
    with TLabel.Create(Result) do
    begin
      Parent := Result;
      Caption := 'Reading RTTI';
      Left := 68;
      Top := 16;
      Font.Size := 10;
      Font.Style := [fsBold];
    end;
    with TButton.Create(Result) do
    begin
      Parent := Result;
      Left := 96;
      Top := 88;
      Caption := 'Cancel';
      OnClick := btnCancelClick;
    end;
    ProgressBar := TProgressBar.Create(Result);
    with ProgressBar do
    begin
      Parent := Result;
      SetBounds(7, 56, 257, 18);
    end;
    Canceled := False;
  end;
end;

procedure TJvLoadProgress.btnCancelClick(Sender: TObject);
begin
  Canceled := True;
end;

procedure TJvLoadProgress.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

//=== TJvRegEditor ===========================================================

constructor TJvRegEditor.Create1(AOwner: TComponent; lComponent: TComponent);
begin
  inherited Create(AOwner);
  Component := lComponent as TJvRegAuto;
  FProps := Component.Props.Text;
  FModified := False;
end;

procedure TJvRegEditor.btnOkClick(Sender: TObject);
begin
  Apply;
  Close;
end;

procedure TJvRegEditor.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TJvRegEditor.Apply;
begin
  Component.Props := List.Items;
end;

procedure TJvRegEditor.FormShow(Sender: TObject);
begin
  LoadProgress := LoadProgressCreate;
  try
    LoadProgress.Show;
    TreeLoad;
  finally
    LoadProgress.Close;
  end;
  ListLoad;
end;

procedure TJvRegEditor.ListLoad;
begin
  List.Items.Assign(Component.Props);
end;

function IsEnabled(S: ShortString): Boolean;
var
  P: Integer;
begin
  P := Pos(#0, S);
  Result := not ((P = 0) or (Length(S) <= P) or (S[P + 1] = 'N'));
end;

procedure TJvRegEditor.TreeLoad;
const
  SOrdType: array [0..5] of PChar =
    ('Shortint', 'Byte', 'Integer', 'Word', 'Longint', 'Cardinal');
  SFloatType: array [0..4] of PChar =
    ('Single', 'Double', 'Extended', 'Comp', 'Currency');
var
  I, J: Integer;
  ANode: TTreeNode;
  TypeInf: PTypeInfo;
  TypeData: PTypeData;
  PropList: PPropList;
  NumProps: word;
  AName: string;
  CompList: TList;
  Comp: TComponent;

  procedure AddToTree(AComponent: TComponent; APropInfo: PPropInfo; Node: TTreeNode);
  var
    ATypeInfo: PTypeInfo;
    aPropList: PPropList;
    aNumProps: Word;
    I: Integer;
    MyNode: TTreeNode;
  begin
    ATypeInfo := APropInfo^.PropType^;
    TypeData := GetTypeData(ATypeInfo);
    MyNode := nil;
    with ATypeInfo^ do
      case Kind of
        tkUnknown:
          begin
            MyNode :=
              Tree.Items.AddChild(Node, 'Unknown - ' + AName + #0 + 'N');
            MyNode.ImageIndex := imUnknown; // Picture - Not possible to select [translated]
          end;
        tkInteger:
          begin
            MyNode :=
              Tree.Items.AddChild(Node, AName + ' : '
              + SOrdType[Integer(TypeData^.OrdType)] + #0 + 'Y');
            MyNode.ImageIndex := imInteger; // Picture - Possible to select [translated]
          end;
        tkFloat:
          begin
            MyNode :=
              Tree.Items.AddChild(Node, AName + ' : '
              + SFloatType[Integer(TypeData^.FloatType)] + #0 + 'Y');
            MyNode.ImageIndex := imInteger; // Picture - Possible to select [translated]
          end;
        tkWChar,
          tkChar:
          begin
            MyNode :=
              Tree.Items.AddChild(Node, AName + ' : '
              + System.Copy(GetEnumName(TypeInfo(TTypeKind)
              , Integer(Kind)), 3, 255) + #0 + 'Y');
            MyNode.ImageIndex := imChar; // Picture - Possible to select [translated]
          end;
        tkEnumeration:
          begin
            MyNode :=
              Tree.Items.AddChild(Node, AName + ' : '
              + ATypeInfo^.Name + #0 + 'Y');
            MyNode.ImageIndex := imEnumeration; // Picture - Possible to select [translated]
          end;
        tkString, tkLString {, tkWString}:
          begin
            MyNode :=
              Tree.Items.AddChild(Node, AName + ' : '
              + ATypeInfo^.Name + ' ( '
              + Format('String[%d]', [TypeData^.MaxLength])
              + ' )' + #0 + 'Y');
            MyNode.ImageIndex := imString; // Picture - Possible to select [translated]
          end;
        tkClass:
          begin
            MyNode := Tree.Items.AddChild(Node, AName + ' : '
              + TypeData^.ClassType.ClassName + #0 + 'N');
            MyNode.ImageIndex := imClass; // Picture - Possible to select [translated]
            AComponent := TComponent(GetOrdProp(AComponent, APropInfo));

            { Protection against recursive references of components [translated] }
            if (AComponent = nil) or (CompList.IndexOf(AComponent) > -1) then
              Exit;
            CompList.Add(AComponent);

            aNumProps := TypeData^.PropCount;
            GetMem(aPropList, aNumProps * SizeOf(Pointer));
            try
              GetPropInfos(ATypeInfo, aPropList);
              for I := 0 to aNumProps - 1 do
              begin
                AName := aPropList^[I]^.Name;
                AddToTree(AComponent, aPropList^[I], MyNode);
              end;
            finally
              FreeMem(aPropList, aNumProps * SizeOf(Pointer));
            end;
          end;
        // tkSet    // - Not yet supported [translated]
      end;
    if MyNode <> nil then
      MyNode.SelectedIndex := MyNode.ImageIndex;
  end;

  procedure LoadBitmap;
  var
    Pic, PicTmp: TBitmap;
    s: string;
  begin
    if not (csDesigning in ComponentState) then
      Exit;
    Pic := TBitmap.Create;
    s := Comp.ClassName;
    try
      Pic.LoadFromResourceName(hInstance, UpperCase(pchar(s)));
    except
    end;
    if (Pic.Height <> 24) or (Pic.Width <> 24) then
    begin
      PicTmp := TBitmap.Create;
      PicTmp.Height := 24;
      PicTmp.Width := 24;
      PicTmp.Canvas.Draw(0, 0, Pic);
      Pic.Free;
      Pic := PicTmp;
    end;
    TreeImages.AddMasked(Pic, clOlive);
    Pic.Free;
  end;

begin
  CompList := TList.Create;
  Tree.Items.BeginUpdate;
  try
    Tree.Items.Clear;
    LoadProgress.ProgressBar.Max := Component.Owner.ComponentCount + 2;
    for J := -1 to Component.Owner.ComponentCount - 1 do
    begin
      if J = -1 then
        Comp := Component.Owner
      else
        Comp := Component.Owner.Components[J];

      LoadProgress.ProgressBar.Position := J + 1;
      Application.ProcessMessages;
      if LoadProgress.Canceled then
        Exit;
      //ODS('Read ' + Comp.Name + ':' + Comp.ClassName);

      LoadBitmap;
      ANode := Tree.Items.Add(nil, Comp.Name + ' : ' + Comp.ClassName);
      //    else ANode := Tree.Items.Add(nil, Component.Owner.Components[J].Name +' : '+ Component.Owner.Components[J].ClassName);
      ANode.ImageIndex := imComponent + J + 1;
      ANode.SelectedIndex := ANode.ImageIndex;
      try
        TypeInf := Comp.ClassInfo;
        AName := TypeInf^.Name;
        TypeData := GetTypeData(TypeInf);
        NumProps := TypeData^.PropCount;
        GetMem(PropList, NumProps * SizeOf(Pointer));
        try
          GetPropInfos(TypeInf, PropList);
          for I := 0 to NumProps - 1 do
          begin
            AName := PropList^[I]^.Name;
            CompList.Clear;
            CompList.Add(Comp);
            AddToTree(Comp, PropList^[I], ANode);
          end;
        finally
          FreeMem(PropList, NumProps * SizeOf(Pointer));
        end;
      except
        on E: Exception do
        begin
          E.Message := 'JvRegAutoEditorForm error:' + E.Message;
          raise;
        end;
      end;
    end;
    //  Tree.AlphaSort;
  finally
    CompList.Free;
    Tree.Items.EndUpdate;
  end;
end;

procedure TJvRegEditor.FormResize(Sender: TObject);
begin
  edtProp.Width := panelButtons.Left - edtProp.Left * 2 - 2;
end;

procedure TJvRegEditor.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  inherited;
  Msg.MinMaxInfo^.ptMinTrackSize.X := panelOKCancelApply.Width + 15;
  Msg.MinMaxInfo^.ptMinTrackSize.Y := 200;
end;

procedure TJvRegEditor.TreeChange(Sender: TObject; Node: TTreeNode);
var
  Text, Text1: string;
  P: Integer;
begin
  if Node = nil then
    Exit;
  Text := Node.Text;
  btnAddProp.Enabled := IsEnabled(Text);
  P := Pos(' ', Text);
  if P > 0 then
    Text := System.Copy(Text, 1, P - 1);
  Node := Node.Parent;
  while Node <> nil do
  begin
    Text1 := Node.Text;
    P := Pos(' ', Text1);
    if P > 0 then
      Text1 := System.Copy(Text1, 1, P - 1);
    Text := Text1 + '.' + Text;
    Node := Node.Parent;
  end;
  edtProp.Text := Text;
  {  Ind := List.Items.IndexOf(edtProp.Text);
    if Ind <> -1 then List.ItemIndex := Ind;}
end;

procedure TJvRegEditor.btnAddPropClick(Sender: TObject);
var
  Ind: Integer;
begin
  Ind := List.Items.IndexOf(edtProp.Text);
  if Ind = -1 then
    PropAdd
  else
  if ActiveControl = List then
    PropDelete
  else
  begin
    List.Items.Delete(Ind);
    List.ItemIndex := Ind;
  end;
end;

procedure TJvRegEditor.PropAdd;
begin
  if List.Items.IndexOf(edtProp.Text) = -1 then
  begin
    List.Items.Add(edtProp.Text);
    List.ItemIndex := List.Items.IndexOf(edtProp.Text);
  end;
end;

procedure TJvRegEditor.PropDelete;
var
  It: Integer;
begin
  It := List.ItemIndex;
  List.Items.Delete(List.ItemIndex);
  if It > List.Items.Count - 1 then
    dec(It);
  if It < 0 then
    It := 0;
  List.ItemIndex := It;
  if List.Items.Count <> 0 then
    edtProp.Text := List.Items[List.ItemIndex];
end;

procedure TJvRegEditor.ListKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    PropDelete;
  if List.Items.Count <> 0 then
    edtProp.Text := List.Items[List.ItemIndex];
end;

procedure TJvRegEditor.ListClick(Sender: TObject);
begin
  edtProp.Text := List.Items[List.ItemIndex];
end;

procedure TJvRegEditor.TreeEnter(Sender: TObject);
begin
  TreeChange(Sender, Tree.Selected);
end;

procedure TJvRegEditor.ListDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  Offset: Integer; { text offset width }
  BitmapIndex: Integer;
  ComponentName: string[100];
  Obj: TComponent;
begin
  (Control as TListBox).Canvas.FillRect(Rect); { clear the rectangle }

  if Tree.Images <> nil then
  begin
    ComponentName := (Control as TListBox).Items[Index];
    ComponentName := Copy(ComponentName, 1, Pos('.', ComponentName) - 1);
    if ComponentName = Component.Owner.Name then
      ComponentName := '';
    if ComponentName = '' then
      BitmapIndex := 0
    else
    begin
      Obj := Component.Owner.FindComponent(ComponentName);
      if Obj <> nil then
        BitmapIndex := Obj.ComponentIndex + 1
      else
        BitmapIndex := imUnknown;
    end;
    inc(BitmapIndex, imComponent);

    TreeImages.Draw((Control as TListBox).Canvas, Rect.Left + 2, Rect.Top, BitmapIndex);
    Offset := TreeImages.width + 6; { add four pixels between bitmap and text }
  end
  else
    Offset := 2;
  (Control as TListBox).Canvas.TextOut(Rect.Left + Offset, Rect.Top, (Control as TListBox).Items[Index]);
    { display the text }
  //Note that the Rect parameter automatically provides the proper location of the item within the control's canvas.
end;

procedure TJvRegEditor.FormCreate(Sender: TObject);
begin
  //  if Tree.Images = nil then List.Style := lbStandard;
  with TSplitter.Create(Self) do
  begin
    Parent := Self;
    Align := alLeft;
    Left := 201;
    Beveled := False;
    Visible := True;
  end;
  with TJvRegAuto.Create(Self) do
  begin
    {$IFDEF DELPHI}
    RegPath := 'Software\Borland\Delphi\JVCL\JvRegAutoEditorForm';
    {$ENDIF DELPHI}
    {$IFDEF BCB}
    RegPath := 'Software\Borland\C++Builder\JVCL\JvRegAutoEditorForm';
    {$ENDIF BCB}
    AutoMode := True;
    SaveWindowPlace := True;
    Props.Add('Tree.Width');
    Load;
  end;
  edtProp.Hint := sRegAutoEditorEdtPropHint;
  Tree.Hint := sRegAutoEditorTreeHint;
  List.Hint := sRegAutoEditorListHint;
  btnAddProp.Hint := sRegAutoEditorBtnAddPropHint;
  Sort1.Caption := sRegAutoEditorSort;
end;

procedure TJvRegEditor.edtPropChange(Sender: TObject);
var
  Ind: Integer;
begin
  Ind := List.Items.IndexOf(edtProp.Text);
  if Ind <> -1 then
    List.ItemIndex := Ind;
end;

procedure TJvRegEditor.edtPropKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ^M then
  begin
    btnAddPropClick(Sender);
    Key := #0;
  end;
end;

procedure TJvRegEditor.ListDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = Tree) and (List.Items.IndexOf(edtProp.Text) = -1);
end;

procedure TJvRegEditor.ListDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if (Source = Tree) and (List.Items.IndexOf(edtProp.Text) = -1) then
    btnAddPropClick(Self);
  //  then PropAdd;
end;

procedure TJvRegEditor.TreeDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = List) and (List.Items.IndexOf(edtProp.Text) <> -1);
end;

procedure TJvRegEditor.TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if (Source = List) and (List.Items.IndexOf(edtProp.Text) <> -1) then
    btnAddPropClick(Self);
  //  then PropDelete;
end;

procedure TJvRegEditor.ListEnter(Sender: TObject);
begin
  btnAddProp.Enabled := True;
end;

procedure TJvRegEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FProps <> Component.Props.Text then
    FModified := True;
end;

procedure TJvRegEditor.Sort1Click(Sender: TObject);
begin
  List.Sorted := True;
  //  List.Sorted := False;
end;

function GetProjectName: string;
begin
  if Assigned(ToolServices) then
    Result := ToolServices.GetProjectName
  else
    Result := '';
end;

initialization
  GetProjectNameProc := GetProjectName;
  
end.


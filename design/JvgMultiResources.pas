{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgMultiResources.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgMultiResources;

interface

uses
  Windows, Controls, Classes, Forms, SysUtils,
  Dialogs, TypInfo,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, PropertyCategories,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvComponent;

type
  TJvgResStringList = class(TStringList);

  TJvgMultipleResources = class(TJvComponent)
  private
    FComps: TStringList;
    FResources: TJvgResStringList;
    //    FCompList: TList;
//    function GetName(Component: TComponent): string;
//    procedure ProcessNewComponent(Component: TComponent);
    procedure GetComponentData(C: TComponent; SLNames: TStringList);
    //    procedure GetPropEditor(Prop: TPropertyEditor);
    //    procedure E(const S: string);
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    //    property CompList: TList read FCompList write FCompList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update;
  published
    property Resources: TJvgResStringList read FResources write FResources;
    property Comps: TStringList read FComps write FComps;
  end;

implementation

uses
  StdCtrls, Contnrs; //, JvgMultiResourceEditor, JvDsgnTypes;

constructor TJvgMultipleResources.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  FResources := TJvgResStringList.Create;
  FComps := TStringList.Create;
  //  FCompList := TList.Create;
  {  FResources.Add('Form1#Label1#Caption1');
    FResources.Add('Form2#Label2#Caption2');
    FResources.Add('Form3#Label3#Caption3');
    FResources.Add('Form4#Label4#Caption4');}
  if (csDesigning in ComponentState) and (FComps.Count = 0) then
    with TForm(AOwner) do
    begin
      for I := 0 to ControlCount - 1 do
        if TComponent(Comps[I]) is TLabel then
          FComps.Add(TComponent(Comps[I]).Name);
      //FResources.Add( Comps[I].Name+'#'+TLabel(Comps[I]).Caption );
    end;
end;

destructor TJvgMultipleResources.Destroy;
begin
  FComps.Free;
  FResources.Free;
  // FCompList.Free;
  inherited Destroy;
end;

procedure TJvgMultipleResources.Notification(Component: TComponent;
  Operation: TOperation);
begin
  inherited Notification(Component, Operation);
  //  if (Component <> Self)and(Operation = opInsert) then ProcessNewComponent(Component);
    //if (Component <> Self)and(Operation = opRename)and(Assigned(LastComponent)) then ProcessNewComponent(Component);
  //    raise Exception.Create('Cannot create more than one instance of TJvgMultipleResources component');
  if (Component <> Self) and (Operation = opInsert) then
    if (Component is TLabel) or (Component is TListBox) then
    begin
      Comps.AddObject(Component.name, Component);
      Resources.Add('');
    end;
  {  if (Component <> Self)and(Operation = opRemove) then
      for I := 0 to CompList.Count-1 do if CompList[I] = Component then
      begin
        CompList.Delete(I);
        //for J := Comps.Count-1 downto 0 do
        //  if Pos( Component.Name Comps[] )
        Break;
      end;}
end;

procedure TJvgMultipleResources.Update;
var
  I, J, Ind: Integer;
  C: TComponent;
  NameOfC: string;
  SLNames: TStringList;
begin
  SLNames := TStringList.Create;
  try
    for I := 0 to Comps.Count - 1 do
    begin
      NameOfC := '';
      C := TComponent(Comps.Objects[I]);
      if C = nil then
      begin
        for J := 1 to length(Comps[I]) do
          if Comps[I][J] = '.' then
            Break;
        NameOfC := Copy(Comps[I], 0, J - 1);
        C := Owner.FindComponent(NameOfC);
      end;
      if C = nil then
        Exit;

      if NameOfC <> '' then
        for J := 0 to Comps.Count - 1 do
          if Pos(NameOfC + '.', Comps[I]) = 1 then
            Comps.Objects[I] := C;

      if C.Name = '' then
        Continue;
      SLNames.Clear; //SLNames.Sorted := True;
      GetComponentData(C, SLNames);
      for J := 0 to SLNames.Count - 1 do
      begin
        //IndexOfObject(
        Ind := Comps.IndexOf(C.Name + '.' + SLNames.Names[J]);
        if Ind = -1 then
        begin
          Comps.Insert(Comps.IndexOfObject(C), C.Name + '.' +
            SLNames.Names[J]);
          Resources.Add(SLNames.Values[SLNames.Names[J]]);
        end
        else
          Resources[Ind] := SLNames.Values[SLNames.Names[J]];
      end;
      if Comps[Comps.IndexOfObject(C)] = '' then
      begin
        Resources.Delete(Comps.IndexOfObject(C) - 1);
        Comps.Delete(Comps.IndexOfObject(C));
      end;
    end;
  finally
    SLNames.Free;
  end;
end;

procedure TJvgMultipleResources.GetComponentData(C: TComponent;
  SLNames: TStringList);
var
  I: Integer;
begin
  if C is TLabel then
    SLNames.Add('Caption=' + TLabel(C).Caption)
  else
  if C is TListBox then
    for I := 0 to TListBox(C).Items.Count - 1 do
      SLNames.Add('Items[' + IntToStr(I) + ']=' + TListBox(C).Items[I]);
end;

procedure TJvgMultipleResources.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
  begin
  end;
end;

(*
function TJvgMultipleResources.GetName(Component: TComponent): string;
var
  I: Integer;
  BaseName: string;
begin
  if Component.Name <> '' then
  begin
    Result := Component.Name;
    Exit;
  end;
  I := 1;
  BaseName := Copy(Component.ClassName, 2, length(Component.ClassName));
  while Owner.FindComponent(BaseName + IntToStr(I)) <> nil do
    inc(I);
  Result := BaseName + IntToStr(I);
end;
*)

(*
procedure TJvgMultipleResources.ProcessNewComponent(Component: TComponent);
var
  SLNames, SLValues: TStringList;
  CompList: TComponentList;
begin
  // (p3) I don't see how this codse will ever work...
  if not (Component is TControl) then
    Exit;
  SLNames := TStringList.Create;
  SLValues := TStringList.Create;
  //  CompList := TComponentList.Create;
  //  FormDesigner := IJvFormDesigner.Create;
  if (Component is TLabel) or (Component is TListBox) then
    CompList.Add(Component);

  try
    {    if Component is TLabel then
          Comps.Add( GetName( Component ) );
        if Component is TListBox then //with TListBox(Component) do
          for I:=0 to TListBox(Component).Items.Count-1 do
            Comps.Add( GetName( Component )+'.Items['+IntToStr(I)+']' );
    }
  finally
    ///    FormDesigner.Free;
    //    CompList.Free;
    SLNames.Free;
    SLValues.Free;
  end;
end;

procedure TJvgMultipleResources.E(const S: string);
begin
  //  ShowMessage(s);
end;

procedure TJvgMultipleResources.GetPropEditor(Prop: TPropertyEditor);
begin
  //  ShowMessage(s);
end;
*)

end.


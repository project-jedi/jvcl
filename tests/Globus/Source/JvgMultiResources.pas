{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgMultiResources.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

UNIT JvgMultiResources;

INTERFACE

USES Windows,
   Controls,
   Classes,
   Forms,
   SysUtils,
   Dialogs,
   {$IFDEF COMPILER6_UP}
   DesignIntf,
   DesignEditors,
   PropertyCategories,
   {$ELSE}
   DsgnIntf,
   {$ENDIF COMPILER6_UP}
   JVComponent,
   TypInfo;
TYPE

   TJvgResStringList = CLASS(TStringList)
   END;

   TJvgMultipleResources = CLASS(TJvComponent)
   PRIVATE
      FComps: TStringList;
      FResources: TJvgResStringList;
      //    FCompList: TList;
      FUNCTION GetName(Component: TComponent): STRING;
      PROCEDURE ProcessNewComponent(Component: TComponent);
      PROCEDURE GetComponentData(C: TComponent; SLNames: TStringList);
      PROCEDURE GetPropEditor(Prop: TPropertyEditor);
      PROCEDURE E(CONST S: STRING);
   PROTECTED
      PROCEDURE Notification(Component: TComponent; Operation: TOperation);
         OVERRIDE;
      PROCEDURE Loaded; OVERRIDE;
   PUBLIC
      //    property CompList: TList read FCompList write FCompList;
      PROCEDURE Update;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
      PROPERTY Resources: TJvgResStringList READ FResources WRITE FResources;
      PROPERTY Comps: TStringList READ FComps WRITE FComps;
   END;

IMPLEMENTATION
USES StdCtrls,
   Contnrs;                             //, JvgMultiResourceEditor;

{$IFDEF COMPILER6_UP}
TYPE
   TDesigner = DesignIntf.IDesigner;
   TFormDesigner = DesignIntf.IDesigner;
   {$ELSE}
{$IFDEF COMPILER4_UP}
TYPE
   TDesigner = IDesigner;
   TFormDesigner = IFormDesigner;
   {$ENDIF}
   {$ENDIF}

CONSTRUCTOR TJvgMultipleResources.Create(AOwner: TComponent);
VAR
   i, j                       : integer;
BEGIN
   INHERITED;
   FResources := TJvgResStringList.Create;
   FComps := TStringList.Create;
   //  FCompList := TList.Create;
   {  FResources.Add('Form1#Label1#Caption1');
     FResources.Add('Form2#Label2#Caption2');
     FResources.Add('Form3#Label3#Caption3');
     FResources.Add('Form4#Label4#Caption4');}
   IF (csDesigning IN ComponentState) AND (FComps.Count = 0) THEN
      WITH TForm(AOwner) DO
      BEGIN
         FOR i := 0 TO ControlCount - 1 DO
            IF TComponent(Comps[i]) IS TLabel THEN
               FComps.Add(TComponent(Comps[i]).Name);
         //FResources.Add( Comps[i].Name+'#'+TLabel(Comps[i]).Caption );
      END;
END;

DESTRUCTOR TJvgMultipleResources.Destroy;
BEGIN
   FComps.Free;
   FResources.Free;
   // FCompList.Free;
   INHERITED Destroy;
END;

PROCEDURE TJvgMultipleResources.Notification(Component: TComponent; Operation:
   TOperation);
VAR
   i, j                       : integer;
BEGIN
   INHERITED;
   //  if (Component <> Self)and(Operation = opInsert) then ProcessNewComponent(Component);
     //if (Component <> Self)and(Operation = opRename)and(Assigned(LastComponent)) then ProcessNewComponent(Component);
   //    raise Exception.Create('Cannot create more than one instance of TJvgMultipleResources component');
   IF (Component <> Self) AND (Operation = opInsert) THEN
      IF (Component IS TLabel) OR (Component IS TListBox) THEN
      BEGIN
         Comps.AddObject(Component.name, Component);
         Resources.Add('');
      END;
   {  if (Component <> Self)and(Operation = opRemove) then
       for i := 0 to CompList.Count-1 do if CompList[i] = Component then
       begin
         CompList.Delete(i);
         //for j := Comps.Count-1 downto 0 do
         //  if pos( Component.Name Comps[] )
         break;
       end;}
END;

PROCEDURE TJvgMultipleResources.Update;
VAR
   i, j, Ind                  : integer;
   c                          : TComponent;
   cName                      : STRING;
   SLNames                    : TStringList;
BEGIN

   SLNames := TStringList.Create;
   TRY
      FOR i := 0 TO Comps.Count - 1 DO
      BEGIN
         cName := '';
         C := TComponent(Comps.Objects[i]);
         IF C = NIL THEN
         BEGIN
            FOR j := 1 TO length(Comps[i]) DO
               IF Comps[i][j] = '.' THEN
                  break;
            cName := copy(Comps[i], 0, j - 1);
            C := Owner.FindComponent(cName);
         END;
         IF C = NIL THEN
            exit;

         IF cName <> '' THEN
            FOR j := 0 TO Comps.Count - 1 DO
               IF pos(cName + '.', Comps[i]) = 1 THEN
                  Comps.Objects[i] := C;

         IF C.Name = '' THEN
            continue;
         SLNames.Clear;                 //SLNames.Sorted := true;
         GetComponentData(C, SLNames);
         FOR j := 0 TO SLNames.Count - 1 DO
         BEGIN
            //IndexOfObject(
            Ind := Comps.IndexOf(C.Name + '.' + SLNames.Names[j]);
            IF Ind = -1 THEN
            BEGIN

               Comps.Insert(Comps.IndexOfObject(C), C.Name + '.' +
                  SLNames.Names[j]);
               Resources.Add(SLNames.Values[SLNames.Names[j]]);
            END
            ELSE
               Resources[Ind] := SLNames.Values[SLNames.Names[j]];
         END;
         IF Comps[Comps.IndexOfObject(C)] = '' THEN
         BEGIN
            Resources.Delete(Comps.IndexOfObject(C) - 1);
            Comps.Delete(Comps.IndexOfObject(C));
         END;
      END;
   FINALLY
      SLNames.Free;
   END;

END;

PROCEDURE TJvgMultipleResources.GetComponentData(C: TComponent; SLNames:
   TStringList);
VAR
   i                          : integer;
BEGIN
   IF C IS TLabel THEN
   BEGIN
      SLNames.Add('Caption=' + TLabel(C).Caption);
   END
   ELSE IF C IS TListBox THEN
      FOR i := 0 TO TListBox(C).Items.Count - 1 DO
      BEGIN
         SLNames.Add('Items[' + IntToStr(i) + ']=' + TListBox(C).Items[i]);
      END;
END;

PROCEDURE TJvgMultipleResources.Loaded;
BEGIN
   INHERITED;
   IF NOT (csDesigning IN ComponentState) THEN
   BEGIN
   END;
END;

FUNCTION TJvgMultipleResources.GetName(Component: TComponent): STRING;
VAR
   i                          : integer;
   BaseName                   : STRING;
BEGIN
   IF Component.Name <> '' THEN
   BEGIN
      Result := Component.Name;
      exit;
   END;
   i := 1;
   BaseName := copy(Component.ClassName, 2, length(Component.ClassName));
   WHILE Owner.FindComponent(BaseName + IntToStr(i)) <> NIL DO
      inc(i);
   Result := BaseName + IntToStr(i);
END;

PROCEDURE TJvgMultipleResources.ProcessNewComponent(Component: TComponent);
VAR
   SLNames, SLValues          : TStringList;
   FormDesigner               : TFormDesigner;
   CompList                   : TComponentList;
   i                          : integer;
BEGIN
   IF NOT (Component IS TControl) THEN
      exit;
   SLNames := TStringList.Create;
   SLValues := TStringList.Create;
   //  CompList := TComponentList.Create;
   //  FormDesigner := TFormDesigner.Create;
   IF (Component IS TLabel) OR (Component IS TListBox) THEN
      CompList.Add(Component);

   TRY
      {    if Component is TLabel then
            Comps.Add( GetName( Component ) );
          if Component is TListBox then //with TListBox(Component) do
            for i:=0 to TListBox(Component).Items.Count-1 do
              Comps.Add( GetName( Component )+'.Items['+IntToStr(i)+']' );
      }
   FINALLY
      ///    FormDesigner.Free;
      //    CompList.Free;
      SLNames.Free;
      SLValues.Free;
   END;
END;

PROCEDURE TJvgMultipleResources.E(CONST S: STRING);
BEGIN
   //  ShowMessage(s);
END;

PROCEDURE TJvgMultipleResources.GetPropEditor(Prop: TPropertyEditor);
BEGIN
   //  ShowMessage(s);
END;

END.


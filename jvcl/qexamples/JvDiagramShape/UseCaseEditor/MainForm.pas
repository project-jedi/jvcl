{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit MainForm;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QComCtrls, QToolWin, JvQDiagramShape, QImgList, QExtCtrls;

type
  TjimNextAction = (jnaNone, jnaAddActor, jnaAddUseCase,
    jnaStartDouble, jnaEndDouble,
    jnaStartUsesArrow, jnaEndUsesArrow,
    jnaStartExtendsArrow, jnaEndExtendsArrow);

  TjimArrowType = (jatDouble, jatUses, jatExtends);

  TMainDlg = class(TForm)
    ToolBar1: TToolBar;
    ScrollBox1: TScrollBox;
    NewBtn: TToolButton;
    OpenBtn: TToolButton;
    SaveBtn: TToolButton;
    ToolButton5: TToolButton;
    SelectBtn: TToolButton;
    ActorBtn: TToolButton;
    UseCaseBtn: TToolButton;
    DoubleArrowBtn: TToolButton;
    IncludeArrowBtn: TToolButton;
    BtnImageList: TImageList;
    ExtendsArrowBtn: TToolButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    DiagImageList: TImageList;
    ToolButton1: TToolButton;
    DeleteBtn: TToolButton;
    StatusBar: TStatusBar;
    procedure NewBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SelectBtnClick(Sender: TObject);
    procedure ActorBtnClick(Sender: TObject);
    procedure UseCaseBtnClick(Sender: TObject);
    procedure DoubleArrowBtnClick(Sender: TObject);
    procedure IncludeArrowBtnClick(Sender: TObject);
    procedure ExtendsArrowBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ScrollBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DeleteBtnClick(Sender: TObject);
  private
    FNextAction: TjimNextAction;
    FStartShape: TJvCustomDiagramShape;
    FEndShape: TJvCustomDiagramShape;

    procedure SetNextAction(Value: TjimNextAction);
    procedure ChooseButton(TheButton: TToolButton);
    procedure ConnectShapes(StartShape, EndShape: TJvCustomDiagramShape;
      ArrowType: TjimArrowType);
    // The OnClick event handler for all shapes on the diagram
    procedure ShapeClick(Sender: TObject);
    // The OnDblClick event handler for all captions on the diagram
    procedure CaptionDblClick(Sender: TObject);
  public
    property NextAction: TjimNextAction read FNextAction write SetNextAction;
  end;

  EDiagramError = class(Exception);

var
  MainDlg: TMainDlg;

implementation

{$R *.xfm}

uses
  CaptionEditForm;

procedure TMainDlg.SetNextAction(Value: TjimNextAction);
begin
  FNextAction := Value;

  case FNextAction of
    jnaNone: StatusBar.Panels[0].Text := 'Waiting';
    jnaAddActor: StatusBar.Panels[0].Text := 'Place an actor on the diagram';
    jnaAddUseCase: StatusBar.Panels[0].Text := 'Place a use case on the diagram';
    jnaStartDouble: StatusBar.Panels[0].Text := 'Choose the actor';
    jnaEndDouble: StatusBar.Panels[0].Text := 'Choose the use case';
    jnaStartUsesArrow: StatusBar.Panels[0].Text := 'Choose the first use case';
    jnaEndUsesArrow: StatusBar.Panels[0].Text := 'Choose the second use case';
    jnaStartExtendsArrow: StatusBar.Panels[0].Text := 'Choose the first use case';
    jnaEndExtendsArrow: StatusBar.Panels[0].Text := 'Choose the second use case';
  end;
end;

procedure TMainDlg.ChooseButton(TheButton: TToolButton);
var
  i: Integer;
begin {ChooseButton}
  with ToolBar1 do
  begin
    for i := 0 to ControlCount - 1 do
    begin
      if Controls[i] is TToolButton then
        TToolButton(Controls[i]).Down := (Controls[i] = TheButton);
    end;
  end;

  TheButton.Click;
end; {ChooseButton}

procedure TMainDlg.ConnectShapes(StartShape, EndShape: TJvCustomDiagramShape;
  ArrowType: TjimArrowType);
var
  TempConnector: TJvConnector;
  StartSide, EndSide: TJvConnectionSide;
  StartOffset, EndOffset: Integer;
begin {ConnectShapes}
  TempConnector := nil;
  StartSide := csRight;
  EndSide := csLeft;
  StartOffset := FStartShape.Height div 2;
  EndOffset := FEndShape.Height div 2;

  case ArrowType of
    jatDouble:
      begin
        TempConnector := TJvDoubleHeadArrow.Create(Self);
      end;

    jatUses, jatExtends:
      begin
        TempConnector := TJvBluntSingleHeadArrow.Create(Self);
        // Change the connection sides to top and bottom
        StartSide := csBottom;
        EndSide := csTop;
        StartOffset := FStartShape.Width div 2;
        EndOffset := FEndShape.Width div 2;

        // Create the caption
        TempConnector.Caption := TJvTextShape.Create(Self);
        TempConnector.Caption.OnDblClick := CaptionDblClick;

        if ArrowType = jatUses then
        begin
          TempConnector.Caption.Text := '<<include>>';
        end
        else
        begin
          TempConnector.Caption.Text := '<<extend>>';
        end;
      end;
  end;

  with TempConnector do
  begin
    // Set the start connection
    StartConn.Side := StartSide;
    StartConn.Offset := StartOffset;
    StartConn.Shape := FStartShape;
    // Set the end connection
    EndConn.Side := EndSide;
    EndConn.Offset := EndOffset;
    EndConn.Shape := FEndShape;
    // Ensure the size is correct
    SetBoundingRect;
    // Ensure the new control is visible
    Parent := ScrollBox1;

    // Align the caption to near the midpoint of the connector, if necessary
    if Assigned(Caption) then
    begin
      Caption.SetBounds(MidPoint.X + 20, MidPoint.Y, Caption.Width, Caption.Height);
    end;
  end;
end; {ConnectShapes}

procedure TMainDlg.ShapeClick(Sender: TObject);
begin {ShapeClick}
  if not (Sender is TJvCustomDiagramShape) then
  begin
    Exit;
  end;

  case FNextAction of
    jnaStartDouble:
      begin
        // Check that it is an allowed shape
        if not (Sender is TJvBitmapShape) then
        begin
          raise EDiagramError.Create('You must join an actor to a use case');
        end;

        FStartShape := TJvCustomDiagramShape(Sender);
        NextAction := jnaEndDouble;
      end;

    jnaEndDouble:
      begin
        // Check that it is an allowed shape
        if Sender = FStartShape then
        begin
          raise EDiagramError.Create('You cannot join an actor to itself, ' +
            'choose a use case instead');
        end
        else if not (Sender is TJvStandardShape) then
        begin
          raise EDiagramError.Create('You must join an actor to a use case');
        end;

        FEndShape := TJvCustomDiagramShape(Sender);
        ConnectShapes(FStartShape, FEndShape, jatDouble);
        ChooseButton(SelectBtn);
      end;

    jnaStartUsesArrow:
      begin
        // Check that it is an allowed shape
        if not (Sender is TJvStandardShape) then
        begin
          raise EDiagramError.Create('You must join a use case to a use case');
        end;

        FStartShape := TJvCustomDiagramShape(Sender);
        NextAction := jnaEndUsesArrow;
      end;

    jnaEndUsesArrow:
      begin
        // Check that it is an allowed shape
        if Sender = FStartShape then
        begin
          raise EDiagramError.Create('You cannot join a use case to itself, ' +
            'choose another use case instead');
        end
        else if not (Sender is TJvStandardShape) then
        begin
          raise EDiagramError.Create('You must join a use case to a use case');
        end;

        FEndShape := TJvCustomDiagramShape(Sender);
        ConnectShapes(FStartShape, FEndShape, jatUses);
        ChooseButton(SelectBtn);
      end;

    jnaStartExtendsArrow:
      begin
        // Check that it is an allowed shape
        if not (Sender is TJvStandardShape) then
        begin
          raise EDiagramError.Create('You must join a use case to a use case');
        end;

        FStartShape := TJvCustomDiagramShape(Sender);
        NextAction := jnaEndExtendsArrow;
      end;

    jnaEndExtendsArrow:
      begin
        // Check that it is an allowed shape
        if Sender = FStartShape then
        begin
          raise EDiagramError.Create('You cannot join a use case to itself, ' +
            'choose another use case instead');
        end
        else if not (Sender is TJvStandardShape) then
        begin
          raise EDiagramError.Create('You must join a use case to a use case');
        end;

        FEndShape := TJvCustomDiagramShape(Sender);
        ConnectShapes(FStartShape, FEndShape, jatExtends);
        ChooseButton(SelectBtn);
      end;
  end;
end; {ShapeClick}

procedure TMainDlg.CaptionDblClick(Sender: TObject);
var
  TempText: string;
  TempFont: TFont;
begin {CaptionDblClick}
  if Sender is TJvTextShape then
  begin
    with TJvTextShape(Sender) do
    begin
      // Use local variables because cannot pass properties as var parameters
      TempText := Text;
      TempFont := Font;
      TCaptionEditDlg.NewCaption(TempText, TempFont);
      Text := TempText;
      Font := TempFont;
    end;
  end;
end; {CaptionDblClick}

procedure TMainDlg.FormCreate(Sender: TObject);
begin
  NextAction := jnaNone;
  FStartShape := nil;
  FEndShape := nil;
end;

procedure TMainDlg.NewBtnClick(Sender: TObject);
begin
  TJvCustomDiagramShape.DeleteAllShapes(ScrollBox1);
end;

procedure TMainDlg.OpenBtnClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    TJvCustomDiagramShape.LoadFromFile(OpenDialog1.FileName, ScrollBox1);
  end;
end;

procedure TMainDlg.SaveBtnClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    TJvCustomDiagramShape.SaveToFile(SaveDialog1.FileName, ScrollBox1);
  end;
end;

procedure TMainDlg.SelectBtnClick(Sender: TObject);
begin
  // Don't add anything to the diagram on the next click on the scrollbox
  NextAction := jnaNone;
end;

procedure TMainDlg.ActorBtnClick(Sender: TObject);
begin
  // Add an actor to the diagram on the next click on the scrollbox
  NextAction := jnaAddActor;
end;

procedure TMainDlg.UseCaseBtnClick(Sender: TObject);
begin
  // Add a use case to the diagram on the next click on the scrollbox
  NextAction := jnaAddUseCase;
end;

procedure TMainDlg.DoubleArrowBtnClick(Sender: TObject);
begin
  // Connect an actor to a use case
  NextAction := jnaStartDouble;
  FStartShape := nil;
  FEndShape := nil;
end;

procedure TMainDlg.IncludeArrowBtnClick(Sender: TObject);
begin
  // Connect 2 use cases
  NextAction := jnaStartUsesArrow;
  FStartShape := nil;
  FEndShape := nil;
end;

procedure TMainDlg.ExtendsArrowBtnClick(Sender: TObject);
begin
  // Connect 2 use cases
  NextAction := jnaStartExtendsArrow;
  FStartShape := nil;
  FEndShape := nil;
end;

procedure TMainDlg.DeleteBtnClick(Sender: TObject);
begin
  TJvCustomDiagramShape.DeleteSelectedShapes(ScrollBox1);
end;

procedure TMainDlg.ScrollBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case FNextAction of
    jnaNone: TJvCustomDiagramShape.UnselectAllShapes(ScrollBox1);

    jnaAddActor:
      begin
        with TJvBitmapShape.Create(Self) do
        begin
          Caption := TJvTextShape.Create(Self);
          Caption.Text := 'New Actor';
          Caption.OnDblClick := CaptionDblClick;
          Images := DiagImageList;
          ImageIndex := 0;
          Top := Y;
          Left := X;
          OnClick := ShapeClick;
          Parent := ScrollBox1;
          AlignCaption(taCenter);
        end;

        ChooseButton(SelectBtn);
      end;

    jnaAddUseCase:
      begin
        with TJvStandardShape.Create(Self) do
        begin
          Caption := TJvTextShape.Create(Self);
          Caption.Text := 'New Use Case';
          Caption.OnDblClick := CaptionDblClick;
          ShapeType := stEllipse;
          Top := Y;
          Left := X;
          OnClick := ShapeClick;
          Parent := ScrollBox1;
          AlignCaption(taCenter);
          Caption.Top := Top + (Height div 2) - (Caption.Height div 2);
        end;

        ChooseButton(SelectBtn);
      end;

    jnaStartDouble,
      jnaEndDouble,
      jnaStartUsesArrow,
      jnaEndUsesArrow,
      jnaStartExtendsArrow,
      jnaEndExtendsArrow:
      begin
        // Shouldn't really get here when doing anything useful, so treat it as
        // clearing the setting
        ChooseButton(SelectBtn);
      end;
  end;
end;

end.


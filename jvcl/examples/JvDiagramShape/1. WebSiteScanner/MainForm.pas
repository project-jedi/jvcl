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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, JvDiagramShape, ComCtrls, ImgList,
  JvComponent, JvUrlGrabbers, JvUrlListGrabber;

type
  TMainDlg = class(TForm)
    ScrollBox1: TScrollBox;
    Panel1: TPanel;
    ImageList1: TImageList;
    ParseBtn: TButton;
    OpenDialog1: TOpenDialog;
    UrlEdit: TEdit;
    Label1: TLabel;
    Panel2: TPanel;
    ProgressBar: TProgressBar;
    StatusLabel: TLabel;
    CancelBtn: TButton;
    Label2: TLabel;
    PageNameLabel: TLabel;
    JvHttpGrabber1: TJvHttpUrlGrabber;
    procedure ParseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure JvHttpGrabber1DoneStream(Sender: TObject; Stream: TStream;
      StreamSize: Integer; Url: string);
  private
    FCurrentUrl: string;
    FParentUrlList: TStringList;
    FNextChildY: Integer;
    FCurrentShape: TJvBitmapShape;

    procedure GetHttpDocument(const Url: string);
    function CreateBitmapShape(Url: string;
      ALeft, ATop, AImageIndex: Integer): TJvBitmapShape;
    procedure ConnectShapes(StartShape, EndShape: TJvCustomDiagramShape);
    procedure ClearAll(ExceptShape: TJvBitmapShape);
    procedure ParseDoc(const Doc: string);
    function GetLastParentUrl: string;
    procedure RemoveLastParentUrl;
    procedure AddParentUrl(const NewUrl: string);

    // Event handlers that will be assigned to diagram components
    procedure ParentDblClick(Sender: TObject);
    procedure CurrentDblClick(Sender: TObject);
    procedure ChildDblClick(Sender: TObject);
  public
  end;

var
  MainDlg: TMainDlg;

implementation

{$R *.DFM}

uses
  JimParse;

const
  ParentX = 10;
  CurrentX = 100;
  ChildX = 300;
  ChildTop = 5;
  ChildDY = 70;


function CheckUrlForSpaces(const Url: string): string;
var
  i: Integer;
begin {CheckUrlForSpaces}
  // Replace all occurences of '%20' with a space
  Result := Url;
  i := Pos('%20', Result);

  while i > 0 do
  begin
    Delete(Result, i, 3);
    Insert(' ', Result, i);
    i := Pos('%20', Result);
  end;
end; {CheckUrlForSpaces}


procedure TMainDlg.GetHttpDocument(const Url: string);
begin {GetHttpDocument}
  // Request the HTML document
  JvHttpGrabber1.Url := URL;
  JvHttpGrabber1.Start;
  //when it's done, the OnDoneStream will be started
end; {GetHttpDocument}


function TMainDlg.CreateBitmapShape(Url: string;
  ALeft, ATop, AImageIndex: Integer): TJvBitmapShape;
begin {CreateBitmapShape}
  Result := TJvBitmapShape.Create(Self);

  with Result do
  begin
    Top := ATop;
    Left := ALeft;
    Images := ImageList1;
    ImageIndex := AImageIndex;
    Hint := Url;
    ShowHint := True;
    Parent := ScrollBox1;
    // Create a new text shape for the caption
    Caption := TJvTextShape.Create(Self);
    Caption.Parent := ScrollBox1;
    Caption.Text := Url;
  end;
end; {CreateBitmapShape}


procedure TMainDlg.ConnectShapes(StartShape, EndShape: TJvCustomDiagramShape);
begin {ConnectShapes}
  with TJvSingleHeadArrow.Create(Self) do
  begin
    // Set the start connection
    StartConn.Side := csRight;
    StartConn.Offset := StartShape.Height div 2;
    StartConn.Shape := StartShape;
    // Set the end connection
    EndConn.Side := csLeft;
    EndConn.Offset := EndShape.Height div 2;
    EndConn.Shape := EndShape;
    // Ensure the size is correct
    SetBoundingRect;
    // Ensure the new control is visible
    Parent := ScrollBox1;
  end;
end; {ConnectShapes}


procedure TMainDlg.ClearAll(ExceptShape: TJvBitmapShape);
var
  i: Integer;
begin {ClearAll}
  // Free all the diagram components
  with ScrollBox1 do
  begin
    i := 0;

    while i < ControlCount do
    begin
      if (Controls[i] is TJvCustomDiagramShape) and
        Assigned(ExceptShape) and
        (Controls[i] <> ExceptShape) and
        (Controls[i] <> ExceptShape.Caption) then
        begin
        // Only want to delete the diagram controls. But DO NOT want to free
        // the current control because we are probably in its on click event
        // handler, and freeing the control will cause all sorts of problems
        // when the event handler tries to exit
        Controls[i].Free;
      end else
      begin
        Inc(i);
      end;
    end;
  end;

  // Reset the starting point for the child page components
  FNextChildY := ChildTop;
end; {ClearAll}


procedure TMainDlg.ParseDoc(const Doc: string);
var
  i: Integer;
  TempStr: string;
  BaseStr: string;
  TempIndex: Integer;
  IsLink: Boolean;
  ParentShape, CurrShape, ChildShape: TJvCustomDiagramShape;
begin {ParseDoc}
  BaseStr := '';

  with TjimHtmlParser.Create do
  begin
    try
      if FCurrentUrl = '' then
      begin
        ClearAll(nil);
        Exit;
      end;

      Parse(Doc);
      // Successfully parsed the document, so clear the current display
      ClearAll(FCurrentShape);

      // Create the parent and current document components
      if FCurrentShape = nil then
      begin
        CurrShape := CreateBitmapShape(FCurrentUrl, CurrentX, ScrollBox1.Height div 2, 0);
      end else
      begin
        CurrShape := FCurrentShape;
        CurrShape.SetBounds(CurrentX, ScrollBox1.Height div 2,
          CurrShape.Width, CurrShape.Height);
      end;

      CurrShape.OnDblClick := CurrentDblClick;

      if GetLastParentUrl > '' then
      begin
        ParentShape := CreateBitmapShape(GetLastParentUrl, ParentX, ScrollBox1.Height div 2, 0);
        ParentShape.OnDblClick := ParentDblClick;
        // Connect the parent to the current document
        ConnectShapes(ParentShape, CurrShape);
      end;

      StatusLabel.Caption := 'Drawing';
      ProgressBar.Position := 0;
      ProgressBar.Max := SymbolTable.Count;

      // Step through symbol table, showing what has been found
      for i := 0 to SymbolTable.Count - 1 do
      begin
        TempStr := SymbolTable.Items[i].SymbolValue;

        case SymbolTable.Items[i].SymbolType of
          stTitle:
            begin
              PageNameLabel.Caption := TempStr;
            end;

          stBase:
            begin
              // Replace any %20 in URL with spaces. Also, this tag should appear
              // before any other links in the document, so can use it to find
              // other URLs.
              BaseStr := CheckUrlForSpaces(TempStr);
            end;

          stLink:
            begin
              // Replace any %20 in URL with spaces
              TempStr := BaseStr + CheckUrlForSpaces(TempStr);
              IsLink := False;

              // Determine the image to use, depening on the URL type
              if StrLIComp('ftp://', PChar(TempStr), 6) = 0 then
              begin
                TempIndex := 2;
              end else if StrLIComp('mailto:', PChar(TempStr), 7) = 0 then
              begin
                TempIndex := 3;
              end else if StrLIComp('news:', PChar(TempStr), 5) = 0 then
              begin
                TempIndex := 4;
              end else if StrLIComp('file://', PChar(TempStr), 7) = 0 then
              begin
                TempIndex := 0;
              end else
              begin
                TempIndex := 0;
                IsLink := True;

                if StrLIComp('http://', PChar(TempStr), 7) <> 0 then
                begin
                  // Trying to load a document with a relative path to the
                  // current one. Make the path absolute.
                  if not ((FCurrentUrl[Length(FCurrentUrl)] in ['/', '\']) or
                    ((Length(TempStr) > 0) and (TempStr[1] in ['/', '\']))) then
                  begin
                    TempStr := '/' + TempStr;
                  end;

                  TempStr := FCurrentUrl + TempStr;
                end;
              end;

              // Create diagram component for this URL, and link to diagram
              // component for current URL
              ChildShape := CreateBitmapShape(TempStr, ChildX, FNextChildY, TempIndex);
              Inc(FNextChildY, ChildDY);
              // Connect this shape to the current document component
              ConnectShapes(CurrShape, ChildShape);

              if IsLink and Assigned(ChildShape) then
              begin
                ChildShape.OnDblClick := ChildDblClick;
              end;
            end;

          stImage:
            begin
              // Replace any %20 in URL with spaces
              TempStr := BaseStr + CheckUrlForSpaces(TempStr);
              // Create diagram component for this URL, and link to diagram
              // component for current URL
              ChildShape := CreateBitmapShape(TempStr, ChildX, FNextChildY, 1);
              Inc(FNextChildY, ChildDY);
              // Connect this shape to the current document component
              ConnectShapes(CurrShape, ChildShape);
            end;
        end;

        ProgressBar.Position := i + 1;
      end;

      Application.ProcessMessages;
    finally
      StatusLabel.Caption := 'Finished';
      ProgressBar.Position := 0;
      Free;
    end;
  end;
end; {ParseDoc}


function TMainDlg.GetLastParentUrl: string;
begin {GetLastParentUrl}
  Result := '';

  if FParentUrlList.Count > 0 then
  begin
    Result := FParentUrlList[FParentUrlList.Count - 1];
  end;
end; {GetLastParentUrl}


procedure TMainDlg.RemoveLastParentUrl;
begin {RemoveLastParentUrl}
  if FParentUrlList.Count > 0 then
  begin
    FParentUrlList.Delete(FParentUrlList.Count - 1);
  end;
end; {RemoveLastParentUrl}


procedure TMainDlg.AddParentUrl(const NewUrl: string);
begin {AddParentUrl}
  FParentUrlList.Add(NewUrl);
end; {AddParentUrl}


procedure TMainDlg.ParentDblClick(Sender: TObject);
begin {ParentDblClick}
  if Sender is TJvBitmapShape then
  begin
    FCurrentShape := TJvBitmapShape(Sender);
  end;

  // Ensure that the parent becomes the current URL
  FCurrentUrl := GetLastParentUrl;
  RemoveLastParentUrl;
  GetHttpDocument(FCurrentUrl);
end; {ParentDblClick}


procedure TMainDlg.CurrentDblClick(Sender: TObject);
begin {CurrentDblClick}
  // Do nothing in this demo. Could fire up an HTML editor
end; {CurrentDblClick}


procedure TMainDlg.ChildDblClick(Sender: TObject);
begin {ChildClick}
  if Sender is TJvBitmapShape then
  begin
    // Ensure that the child becomes the current URL
    FCurrentShape := TJvBitmapShape(Sender);
    AddParentUrl(FCurrentUrl);
    FCurrentUrl := TJvBitmapShape(Sender).Caption.Text;
    GetHttpDocument(FCurrentUrl);
  end;
end; {ChildDblClick}


procedure TMainDlg.FormCreate(Sender: TObject);
begin
  FCurrentUrl := '';
  FParentUrlList := TStringList.Create;
end;


procedure TMainDlg.FormDestroy(Sender: TObject);
begin
  FParentUrlList.Free;
end;


procedure TMainDlg.ParseBtnClick(Sender: TObject);
begin
  FParentUrlList.Clear;
  FCurrentUrl := UrlEdit.Text;
  FCurrentShape := nil;
  GetHttpDocument(FCurrentUrl);
end;


procedure TMainDlg.CancelBtnClick(Sender: TObject);
begin
  JvHttpGrabber1.Stop;
end;


procedure TMainDlg.JvHttpGrabber1DoneStream(Sender: TObject;
  Stream: TStream; StreamSize: Integer; Url: string);
var
  StrStream: TStringStream;
begin
  StrStream:= TStringStream.Create('');
  StrStream.CopyFrom(Stream, Stream.Size);

  // Got whole HTML document, so parse it and display the new map
  try
        ParseDoc(StrStream.DataString);
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
      // Try to recover from parsing errors by stepping back through parent list
      ParentDblClick(Self);
    end;
  end

end;

end.


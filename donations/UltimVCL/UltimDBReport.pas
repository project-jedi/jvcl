{
-----------------------------------------------------------------------------

                              UltimDBReport 1.1

                       (C) Frédéric Leneuf-Magaud 2001

-----------------------------------------------------------------------------

 Please read ReadMe.txt for conditions of use

 This unit is provided 'as-is', without any express or implied warranty.
 In no event shall the author be held liable for any damages arising from the
 use of this unit.

-----------------------------------------------------------------------------

History of changes:

   1.1 : 07/03/2001
         - Added copyright notice
         - Fixed bad font assignments

-----------------------------------------------------------------------------
}

unit UltimDBReport;

interface

{$I OPTIONS.INC}

uses
  SysUtils, Classes, Graphics, Controls, Printers,
  QuickRpt, QRCtrls, DB, DBGrids;

procedure PreviewReport(Grid: TDBGrid; RepTitle: string;
  DefaultHeaderBandHeight, DefaultDetailBandHeight: Integer);

implementation

procedure PreviewReport;
const
  RowSpacing = 8;
var
  Report: TQuickRep;
  I, Size: Integer;
begin
  if (Grid <> nil) and (Grid.DataSource <> nil) and (Grid.DataSource.DataSet <> nil) then
  begin
    { Report creation }
    Report := TQuickRep.Create(Grid);
    try
      with Report do
      begin
        Parent := TWinControl(Grid);
        Visible := False;
        DataSet := Grid.DataSource.DataSet;
        ReportTitle := Trim(RepTitle);
        Page.Orientation := poPortrait;
        Bands.HasTitle := (ReportTitle <> '');
        if Bands.HasTitle then
          with TQRLabel(Bands.TitleBand.AddPrintable(TQRLabel)) do
          begin
            Alignment := taCenter;
            AlignToBand := True;
            AutoSize := True;
            Caption := ReportTitle;
            Font.Assign(Grid.TitleFont); //1.1
            Font.Color := clBlack;
            Font.Size := Font.Size + 4;
            Font.Style := Font.Style + [fsBold];
            Bands.TitleBand.Height := Round(Height * 1.5);
          end;
        Bands.HasColumnHeader := True;
        Report.Bands.ColumnHeaderBand.Height := DefaultHeaderBandHeight + RowSpacing;
        Bands.HasDetail := True;
        Report.Bands.DetailBand.Height := DefaultDetailBandHeight + RowSpacing;
        Bands.HasPageFooter := True;
        with TQRExpr(Bands.PageFooterBand.AddPrintable(TQRExpr)) do
        begin
          Alignment := taRightJustify;
          AlignToBand := True;
          Expression := 'PageNumber';
          Font.Assign(Grid.Font); //1.1
          Font.Color := clBlack;
          Font.Style := [];
          Top := Round(Height / 2);
          Bands.PageFooterBand.Height := Round(Height * 1.5);
        end;
      end;
      { Report objects creation }
      for I := 0 to Grid.Columns.Count - 1 do
      begin
        if Grid.Columns[I].Visible then
        begin
          Size := Grid.Columns[I].Width;
          { Field names }
          with TQRLabel(Report.Bands.ColumnHeaderBand.AddPrintable(TQRLabel)) do
          begin
            if (Left + Size) > Report.Bands.ColumnHeaderBand.Width then
            begin
              if Report.Page.Orientation = poPortrait then
              begin
                Report.Page.Orientation := poLandscape;
                if (Left + Size) > Report.Bands.ColumnHeaderBand.Width then
                  Break; // Not enough space left! Exit
              end
              else
                Break; // Not enough space left! Exit
            end;
            Alignment := Grid.Columns[I].Title.Alignment;
            AutoSize := False;
            Caption := Grid.Columns[I].Title.Caption;
            Font.Assign(Grid.Columns[I].Title.Font); //1.1
            Font.Color := clBlack;
            Frame.DrawRight := True;
            Frame.DrawBottom := True;
            Height := Report.Bands.ColumnHeaderBand.Height - RowSpacing;
            Width := Size;
          end;
          if Grid.Columns[I].Field is TBlobField then
          begin
            if (Grid.Columns[I].Field.DataType = ftMemo) or
              (Grid.Columns[I].Field.DataType = ftFmtMemo) then
            begin
              { Memo fields }
              with TQRDBRichText(Report.Bands.DetailBand.AddPrintable(TQRDBRichText)) do
              begin
                DataSet := Report.DataSet;
                DataField := Grid.Columns[I].FieldName;
                Alignment := Grid.Columns[I].Alignment;
                AutoStretch := True;
                Font.Assign(Grid.Columns[I].Font); //1.1
                Font.Color := clBlack;
                Height := Report.Bands.DetailBand.Height - RowSpacing;
                Width := Size;
              end;
            end
            else
            if Grid.Columns[I].Field.DataType = ftGraphic then
            begin
              { Graphic fields }
              with TQRDBImage(Report.Bands.DetailBand.AddPrintable(TQRDBImage)) do
              begin
                DataSet := Report.DataSet;
                DataField := Grid.Columns[I].FieldName;
                Center := True;
                Stretch := False;
                Height := Report.Bands.DetailBand.Height - RowSpacing;
                Width := Size;
              end;
            end
            else
              { Any other binary fields }
              with TQRLabel(Report.Bands.DetailBand.AddPrintable(TQRLabel)) do
              begin
                Alignment := Grid.Columns[I].Alignment;
                AutoSize := False;
                Caption := '(BLOB)';
                Font.Assign(Grid.Columns[I].Font); //1.1
                Font.Color := clBlack;
                Height := Report.Bands.DetailBand.Height - RowSpacing;
                Width := Size;
              end;
          end
          else
            { Numeric or text fields }
            with TQRDBText(Report.Bands.DetailBand.AddPrintable(TQRDBText)) do
            begin
              DataSet := Report.DataSet;
              DataField := Grid.Columns[I].FieldName;
              Alignment := Grid.Columns[I].Alignment;
              AutoSize := False;
              Font.Assign(Grid.Columns[I].Font); //1.1
              Font.Color := clBlack;
              Height := Report.Bands.DetailBand.Height - RowSpacing;
              Width := Size;
            end;
        end;
      end;
      { Preview }
      Report.Preview;
    finally
      Report.Free;
    end;
  end;
end;

end.

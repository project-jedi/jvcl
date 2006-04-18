object FrameDirectoryEdit: TFrameDirectoryEdit
  Left = 0
  Top = 0
  Width = 489
  Height = 29
  TabOrder = 0
  TabStop = True
  object lblCaption: TLabel
    Left = 7
    Top = 5
    Width = 47
    Height = 13
    Caption = 'lblCaption'
    FocusControl = edtDirectory
  end
  object btnBrowse: TBitBtn
    Left = 464
    Top = 3
    Width = 22
    Height = 22
    Hint = 'Browse...'
    TabOrder = 1
    OnClick = btnBrowseClick
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000094
      940063CECE00F7F7F70094CEFF0094FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00666666666666
      6666666666666666666666600000000000006611111111111100661345454444
      2100613554545444201061355555454410101355554554520220135555554542
      0220111111111111252061355555555555206135555555333320613555553111
      1116661333331666666666611111666666666666666666666666}
    Margin = 0
  end
  object edtDirectory: TEdit
    Left = 120
    Top = 3
    Width = 344
    Height = 21
    TabOrder = 0
    Text = 'edtDirectory'
    OnExit = edtDirectoryExit
  end
end

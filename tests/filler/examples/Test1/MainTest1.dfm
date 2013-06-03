object Form1: TForm1
  Left = 387
  Top = 227
  Width = 548
  Height = 439
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvFillLabel1: TJvFillLabel
    Left = 20
    Top = 135
    Width = 83
    Height = 13
    Caption = 'JvFillLabel1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    Filler = JvFontFiller1
    Index = 7
  end
  object JvFillListBox1: TJvFillListBox
    Left = 20
    Top = 5
    Width = 116
    Height = 121
    Filler = JvFontFiller1
    ItemHeight = 13
    Style = lbOwnerDrawVariable
    TabOrder = 0
  end
  object JvFillListBox2: TJvFillListBox
    Left = 155
    Top = 10
    Width = 116
    Height = 116
    Filler = JvStringsFiller1
    ItemHeight = 13
    TabOrder = 1
  end
  object JvFontFiller1: TJvFontFiller
    Left = 300
    Top = 20
  end
  object JvStringsFiller1: TJvStringsFiller
    Strings.Strings = (
      'Line 1'
      'Line 2'
      'Line 3'
      'Line 4'
      'Line 5'
      'Line 6'
      'Line 7'
      'Line 8')
    Left = 300
    Top = 75
  end
  object JvTreeFiller1: TJvTreeFiller
    Left = 305
    Top = 160
    Root = <
      item
        Implementers = <
          item
            ClassName = 'TJvTreeFillerItemsManagment'
          end
          item
            ClassName = 'TJvTreeFillerItemsDesigner'
          end
          item
            ClassName = 'TJvCustomFillerItemsImages'
          end>
        Items = <
          item
            ClassName = 'TJvBaseFillerItem'
            ID = 'F2F8F259D7E1684996DDDF31DE8F4D'
            Implementers = <
              item
                ClassName = 'TJvFillerTextItemImpl'
                Caption = 'Text only'
              end>
          end
          item
            ClassName = 'TJvBaseFillerItem'
            ID = 'C666F6F7BBC0B74B82C207C5C503B1'
            Implementers = <
              item
                ClassName = 'TJvFillerTextItemImpl'
                Caption = 'Text and image'
              end
              item
                ClassName = 'TJvFillerImageItemImpl'
                Alignment = taRightJustify
                ImageIndex = 1
                SelectedIndex = 1
              end>
          end
          item
            ClassName = 'TJvBaseFillerItem'
            ID = 'F1E5AF81E41905499B93F88ACAFB95'
            Implementers = <
              item
                ClassName = 'TJvFillerTextItemImpl'
                Caption = 'Text and sub items'
              end>
            SubItems = <
              item
                ClassName = 'TJvTreeFillerItems'
                Implementers = <
                  item
                    ClassName = 'TJvTreeFillerItemsManagment'
                  end
                  item
                    ClassName = 'TJvTreeFillerItemsDesigner'
                  end
                  item
                    ClassName = 'TJvCustomFillerItemsImages'
                  end>
                Items = <
                  item
                    ClassName = 'TJvBaseFillerItem'
                    ID = 'CBF8B7CD0698DA4CAF7D448E14BF13'
                    Implementers = <
                      item
                        ClassName = 'TJvFillerTextItemImpl'
                        Caption = 'Text, image and sub items'
                      end
                      item
                        ClassName = 'TJvFillerImageItemImpl'
                        ImageIndex = 2
                        SelectedIndex = 2
                      end>
                    SubItems = <
                      item
                        ClassName = 'TJvTreeFillerItems'
                        Implementers = <
                          item
                            ClassName = 'TJvTreeFillerItemsManagment'
                          end
                          item
                            ClassName = 'TJvTreeFillerItemsDesigner'
                          end
                          item
                            ClassName = 'TJvCustomFillerItemsImages'
                          end>
                        Items = <
                          item
                            ClassName = 'TJvBaseFillerItem'
                            ID = 'AC3DFCD46B223C478794FE44861F81'
                            Implementers = <
                              item
                                ClassName = 'TJvFillerTextItemImpl'
                                Caption = 'First'
                              end>
                          end
                          item
                            ClassName = 'TJvBaseFillerItem'
                            ID = 'E52653606C9CD749A77DD0BB01737D'
                            Implementers = <
                              item
                                ClassName = 'TJvFillerTextItemImpl'
                                Caption = 'Second'
                              end>
                          end>
                      end>
                  end>
              end>
          end>
      end>
  end
end

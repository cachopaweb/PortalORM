object FrmPrincipal: TFrmPrincipal
  Left = 0
  Top = 0
  Caption = 'Gerador de Classes para o PortalORM'
  ClientHeight = 614
  ClientWidth = 1055
  Color = 7036495
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object panLayoutPrincipal: TPanel
    Left = 0
    Top = 0
    Width = 1055
    Height = 585
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 0
    object panLayoutDireita: TPanel
      Left = 449
      Top = 49
      Width = 596
      Height = 526
      Align = alClient
      BevelOuter = bvNone
      Padding.Top = 5
      TabOrder = 0
      object panDireita: TPanel
        Left = 0
        Top = 5
        Width = 596
        Height = 521
        Align = alClient
        BevelOuter = bvNone
        Color = 4866358
        Padding.Left = 10
        Padding.Top = 10
        Padding.Right = 10
        Padding.Bottom = 10
        ParentBackground = False
        TabOrder = 0
        object MemoResultado: TMemo
          Left = 10
          Top = 70
          Width = 576
          Height = 441
          Align = alClient
          BorderStyle = bsNone
          Color = 3288877
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object Panel1: TPanel
          AlignWithMargins = True
          Left = 13
          Top = 13
          Width = 570
          Height = 54
          Align = alTop
          TabOrder = 1
          object Label1: TLabel
            Left = 12
            Top = 8
            Width = 101
            Height = 16
            Caption = 'Nome da Tabela'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWhite
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label2: TLabel
            Left = 215
            Top = 10
            Width = 105
            Height = 16
            Caption = 'Campo de Busca'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWhite
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object EdtTabela: TEdit
            Left = 12
            Top = 27
            Width = 197
            Height = 21
            TabOrder = 0
          end
          object EdtCampoBusca: TEdit
            Left = 215
            Top = 27
            Width = 130
            Height = 21
            TabOrder = 1
          end
          object panBotaoDireita: TPanel
            AlignWithMargins = True
            Left = 348
            Top = 4
            Width = 218
            Height = 46
            Margins.Left = 0
            Align = alRight
            BevelOuter = bvNone
            Padding.Left = 5
            Padding.Top = 5
            Padding.Right = 5
            TabOrder = 2
            object spbSelecionar: TSpeedButton
              Left = 5
              Top = 5
              Width = 98
              Height = 41
              Align = alLeft
              Caption = 'Gerar Classe'
              Glyph.Data = {
                36090000424D3609000000000000360000002800000018000000180000000100
                2000000000000009000000000000000000000000000000000000000000000000
                00000000000000000000000000000000000015131241544C47F8544C47FF544C
                47FF544C47FF544C47FF544C47FF544C47FF544C47FF544C47FF544C47F81513
                1241000000000000000000000000000000000000000000000000000000000000
                00000000000000000000000000000000000024211F6E625A54F6B1A297FFC0B0
                A4FFC0B0A4FFC0B0A4FFC0B0A4FFC0B0A4FFC0B0A4FFB1A297FF625A54F62421
                1F6E000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000101010228232173574F49F9C0B0
                A4FFC0B0A4FFC0B0A4FFC0B0A4FFC0B0A4FFC0B0A4FF574F49F9282321730101
                0102000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000544C47FFC0B0
                A4FFC0B0A4FFC0B0A4FFC0B0A4FFC0B0A4FFC0B0A4FF564E48F1000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000544C47FFC0B0
                A4FFC0B0A4FFC0B0A4FFC0B0A4FFC0B0A4FFC0B0A4FF60585280000000000000
                00000000000000000000000000000000000000000000000000000A09081E4A44
                41C7554D48FC544C47FF544C47FF544C47FF544C47FF544C47FF544C47FF544C
                47FF544C47FF544C47FF544C47FF544C47FF544C47FF544C47FF544C47FF544C
                47FF544C47FF544C47FF544C47FF554D48FC4A4441C70A09081E4A4441C89995
                92FEEAEAE9FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2
                F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2
                F1FFF2F2F1FFF2F2F1FFF2F2F1FFEAEAE9FF999592FE4A4441C7554D48FCEAEA
                E9FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2
                F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2
                F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFEAEAE9FF554D48FC544C47FFF2F2
                F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2
                F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2
                F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FFF2F2F1FF544C47FF544C47FFF2F2
                F1FFD4D3D2FFA39F9CFFA39F9CFFA39F9CFFA39F9CFFA39F9CFFA39F9CFFA39F
                9CFFA39F9CFFA39F9CFFA39F9CFFA39F9CFFA39F9CFFA39F9CFFA39F9CFFA39F
                9CFFA39F9CFFA39F9CFFA39F9CFFA39F9CFFA39F9CFF544C47FF544C47FFC0B0
                A4FFBE9B6EFFA17B48FFA17B48FFA17B48FFA17B48FFA17B48FFA17B48FFA17B
                48FFA17B48FFA17B48FFA17B48FFA17B48FFA17B48FFA17B48FFA17B48FFA17B
                48FFA17B48FFA17B48FFA17B48FF957C5FFF8A7E76FF5A534DB8544C47FFC0B0
                A4FFD7AD77FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA
                49FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFE6A549FFAA80
                48FFAA8048FFE6A549FFEEAA49FFD7AD77FFC0B0A4FF60585280544C47FFC0B0
                A4FFD7AD77FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA
                49FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFC99349FFDEA049FFAA8048FF685C
                52FF685C52FFAA8048FFDD9F49FFB8966CFFC0B0A4FF60585280544C47FFC0B0
                A4FFD7AD77FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA
                49FFEEAA49FFEEAA49FFEEAA49FFA98049FF61554CFF5F5449FF6D5C48FF7E6F
                61FF7E6F61FF6D5C48FF5E5349FF60554DFF90837AFF60585280544C47FFC0B0
                A4FFD7AD77FFEAA849FF856948FFB38649FFEEAA49FFB18448FF846948FFEAA8
                49FFEEAA49FFEEAA49FFEEAA49FF725F4AFF827263FF8D7B6AFF857465FF9F89
                75FF9F8975FF857465FF8D7B6AFF827263FF6B6159FF60585280544C47FFC0B0
                A4FFD4AB76FF836947FF846948FFEBA849FFEEAA49FFEAA849FF836947FF8369
                47FFEAA849FFEEAA49FFEEAA49FFC69248FF5B5249FFA08B76FF9E8975FF8373
                64FF837364FF9E8975FFA08B76FF5A514AFFA4968CFF60585280544C47FFC0B0
                A4FFA18563FF625447FFE8A649FFEEAA49FFEEAA49FFEEAA49FFE8A649FF6254
                47FFAF8448FFEEAA49FF7D664AFF544C47FF756758FFA08B76FF6F6255FF756A
                60FF756A60FF6F6255FFA08B76FF74675AFF544C47FF5B534CE1544C47FFC0B0
                A4FFD4AB76FF836947FF856948FFEBA849FFEEAA49FFEAA849FF846948FF8369
                47FFEAA849FFEEAA49FF544C47FFA08B76FFA08B76FFA08B76FF594F4AFFD0C8
                C2FFD0C8C2FF594F4AFFA08B76FFA08B76FFA08B76FF544C47FF544C47FFC0B0
                A4FFD7AD77FFEAA849FF846948FFB18448FFEEAA49FFB38649FF856948FFEAA8
                49FFEEAA49FFEEAA49FF7D664AFF544C47FF756758FFA08B76FF6F6255FF756A
                60FF756A60FF6F6255FFA08B76FF74675AFF544C47FF5B534CE1544C47FFC0B0
                A4FFD7AD77FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA
                49FFEEAA49FFEEAA49FFEEAA49FFC69248FF5B5249FFA08B76FF9E8975FF8373
                64FF837364FF9E8975FFA08B76FF5A514AFFA4968CFF60585280544C47FFC0B0
                A4FFD7AD77FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA49FFEEAA
                49FFEEAA49FFEEAA49FFEEAA49FF725F4AFF827263FF8D7B6AFF867565FF9F89
                75FF9F8975FF867565FF8D7B6AFF827263FF6B6159FF60585280544C47FCBAAB
                A0FFC8AF93FFD7AD77FFD7AD77FFD7AD77FFD7AD77FFD7AD77FFD7AD77FFD7AD
                77FFD7AD77FFD7AD77FFD7AD77FF9C8162FF60554DFF5D534CFF695C4FFF7E6F
                61FF7E6F61FF695C4FFF5D534CFF5F554EFF90837AFF5C544E7A47403DC88378
                70FEBAABA0FFC0B0A4FFC0B0A4FFC0B0A4FFC0B0A4FFC0B0A4FFC0B0A4FFC0B0
                A4FFC0B0A4FFC0B0A4FFC0B0A4FFC0B0A4FFA6988EFFB4A599FF90837BFF675C
                53FF675C53FF90837BFFB4A599FFA6988EFFBEAFA3FD262320320A09081E4740
                3DC8544C47FC544C47FF544C47FF544C47FF544C47FF544C47FF544C47FF544C
                47FF544C47FF544C47FF5B534DAF6058528060585280605852805F5751875B53
                4DB95B534DB960575187605852805C544E7A2623203200000000}
              OnClick = spbSelecionarClick
              ExplicitLeft = 99
              ExplicitTop = 6
            end
            object btnSalvar: TSpeedButton
              Left = 108
              Top = 5
              Width = 105
              Height = 41
              Align = alRight
              Caption = 'Salvar Unit'
              Glyph.Data = {
                360C0000424D360C000000000000360000002800000020000000200000000100
                180000000000000C0000120B0000120B00000000000000000000FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBBDBD
                BD777777595959404040404040595959777777BDBDBDFBFBFBFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2F2F27C7C7C1111110000
                000000000000000000000000000000000000000000001111117C7C7CF2F2F2FF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFACACAC1515150000000000000000
                00000000000000000000000000000000000000000000000000000000161616AC
                ACACFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFEFEFE8080800101010000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000001
                0101818181FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFF7F7F7F0000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFACACAC0101010000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000010101ACACACFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFF2F2F21515150000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000161616F2F2F2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFF7B7B7B0000000000000000000000000000000000000000000000
                00848484DFDFDF41414100000000000000000000000000000000000000000000
                00000000000000000000007C7C7CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFBFBFB1111110000000000000000000000000000000000000000008B8B
                8BFFFFFFFFFFFFF5F5F542424200000000000000000000000000000000000000
                0000000000000000000000111111FBFBFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFBCBCBC0000000000000000000000000000000000000000008B8B8BFFFF
                FFFFFFFFDFDFDFFFFFFFF5F5F542424200000000000000000000000000000000
                0000000000000000000000000000BDBDBDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFF7676760000000000000000000000000000000000008B8B8BFFFFFFFFFF
                FF7C7C7C0B0B0BC1C1C1FFFFFFF5F5F542424200000000000000000000000000
                0000000000000000000000000000777777FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFF5858580000000000000000000000000000008B8B8BFFFFFFFFFFFF7C7C
                7C0000000000000B0B0BC1C1C1FFFFFFF5F5F542424200000000000000000000
                0000000000000000000000000000595959FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFF3F3F3F000000000000000000000000505050FFFFFFFFFFFF7C7C7C0000
                000000000000000000000B0B0BC2C2C2FFFFFFF5F5F542424200000000000000
                0000000000000000000000000000404040FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFF3F3F3F000000000000000000000000333333D8D8D87373730000000000
                000000000000000000000000000B0B0BC2C2C2FFFFFFF5F5F542424200000000
                00000000000000000000000000003C3C3CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFF5858580000000000000000000000000000000000000000000000000000
                000000000000000000000000000000000B0B0BC2C2C2FFFFFFF5F5F542424200
                00000000000000000000000000004E4E4EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFF7676760000000000000000000000000000000000000000000000000000
                000000000000000000000000000000000000000C0C0CC2C2C2FFFFFFF5F5F542
                42420000000000000000000000007A7A7AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFBCBCBC0000000000000000000000000000000000000000000000000000
                000000000000000000000000000000000000000000000C0C0CC2C2C2FFFFFFF5
                F5F5424242000000000000000000BABABAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFBFBFB1111110000000000000000000000000000000000000000000000
                000000000000000000000000000000000000000000000000000C0C0CC2C2C2FF
                FFFFF5F5F5424242000000232323FEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFF7B7B7B0000000000000000000000000000000000000000000000
                000000000000000000000000000000000000000000000000000000000C0C0CC3
                C3C3FFFFFFF5F5F54242428B8B8BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFF2F2F21515150000000000000000000000000000000000000000
                000000000000000000000000000000000000000000000000000000000000000C
                0C0CC3C3C3FFFFFFE2E2E2B6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFACACAC0101010000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                00000C0C0CC3C3C35252520A0A0AC0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFF7F7F7F0000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                00000000007F7F7F3C3C3C0000000B0B0BC2C2C2FFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFEFEFE7F7F7F0101010000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000828282FFFFFFF3F3F33C3C3C0000003F3F3FFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFACACAC1515150000000000000000
                000000000000000000000000000000000000000000000000000000001B1B1BAE
                AEAEFFFFFFFFFFFFFFFFFFF3F3F3787878B6B6B6FFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1F1F17B7B7B1111110000
                000000000000000000000000000000000000000000001A1A1A787878F2F2F2FF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBBCBC
                BC7676765858583F3F3F4242425F5F5F7C7C7CBDBDBDFDFDFDFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
              OnClick = btnSalvarClick
              ExplicitLeft = 107
            end
          end
        end
      end
    end
    object panLayoutEsquerda: TPanel
      Left = 10
      Top = 49
      Width = 439
      Height = 526
      Align = alLeft
      BevelOuter = bvNone
      Padding.Top = 5
      Padding.Right = 5
      TabOrder = 1
      object panLeft: TPanel
        Left = 0
        Top = 5
        Width = 434
        Height = 521
        Align = alClient
        BevelOuter = bvNone
        Color = 7036495
        Padding.Left = 10
        Padding.Top = 10
        Padding.Right = 10
        Padding.Bottom = 10
        ParentBackground = False
        TabOrder = 0
        object panLeftTop: TPanel
          Left = 10
          Top = 10
          Width = 414
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          Color = 11829573
          Padding.Left = 10
          Padding.Top = 9
          Padding.Right = 10
          Padding.Bottom = 9
          ParentBackground = False
          TabOrder = 0
          object EdtBancoDeDados: TSearchBox
            Left = 10
            Top = 9
            Width = 328
            Height = 23
            Align = alLeft
            TabOrder = 0
            OnInvokeSearch = EdtBancoDeDadosInvokeSearch
          end
          object BtnConectar: TBitBtn
            Left = 344
            Top = 9
            Width = 60
            Height = 23
            Align = alRight
            Caption = 'Conectar'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = BtnConectarClick
          end
        end
        object panLeftGridFields: TPanel
          Left = 10
          Top = 362
          Width = 414
          Height = 149
          Align = alClient
          BevelOuter = bvNone
          Color = 4866358
          Padding.Left = 5
          Padding.Top = 5
          Padding.Right = 5
          Padding.Bottom = 5
          ParentBackground = False
          TabOrder = 1
          object DBGrid2: TDBGrid
            Left = 5
            Top = 5
            Width = 404
            Height = 139
            Align = alClient
            DataSource = DSCampos
            Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = []
            Columns = <
              item
                Expanded = False
                FieldName = 'COLUMN_NAME'
                Title.Alignment = taRightJustify
                Title.Caption = 'Campo'
                Title.Font.Charset = DEFAULT_CHARSET
                Title.Font.Color = clWindowText
                Title.Font.Height = -11
                Title.Font.Name = 'Tahoma'
                Title.Font.Style = [fsBold]
                Width = 165
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'COLUMN_TYPENAME'
                Title.Alignment = taRightJustify
                Title.Caption = 'Tipo'
                Title.Font.Charset = DEFAULT_CHARSET
                Title.Font.Color = clWindowText
                Title.Font.Height = -11
                Title.Font.Name = 'Tahoma'
                Title.Font.Style = [fsBold]
                Width = 140
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'COLUMN_LENGTH'
                Title.Alignment = taRightJustify
                Title.Caption = 'Tamanho'
                Title.Font.Charset = DEFAULT_CHARSET
                Title.Font.Color = clWindowText
                Title.Font.Height = -11
                Title.Font.Name = 'Tahoma'
                Title.Font.Style = [fsBold]
                Width = 60
                Visible = True
              end>
          end
        end
        object panLeftListaTabelas: TPanel
          Left = 10
          Top = 108
          Width = 414
          Height = 16
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Lista das Tabelas'
          Color = 7036495
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentBackground = False
          ParentFont = False
          TabOrder = 2
        end
        object Panel2: TPanel
          Left = 10
          Top = 346
          Width = 414
          Height = 16
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Lista dos Campos da Tabela'
          Color = 7036495
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentBackground = False
          ParentFont = False
          TabOrder = 3
        end
        object panLeftGridTabelas: TPanel
          Left = 10
          Top = 124
          Width = 414
          Height = 222
          Align = alTop
          BevelOuter = bvNone
          Color = 4866358
          Padding.Left = 5
          Padding.Top = 5
          Padding.Right = 5
          Padding.Bottom = 5
          ParentBackground = False
          TabOrder = 4
          object DBGrid1: TDBGrid
            Left = 5
            Top = 5
            Width = 404
            Height = 212
            Align = alClient
            DataSource = DSTabelas
            Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = []
            OnCellClick = DBGrid1CellClick
            Columns = <
              item
                Expanded = False
                FieldName = 'TABLE_NAME'
                Title.Alignment = taCenter
                Title.Caption = 'Tabela'
                Title.Font.Charset = DEFAULT_CHARSET
                Title.Font.Color = clWindowText
                Title.Font.Height = -11
                Title.Font.Name = 'Tahoma'
                Title.Font.Style = [fsBold]
                Width = 370
                Visible = True
              end>
          end
        end
        object Panel3: TPanel
          Left = 10
          Top = 67
          Width = 414
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          Color = 11829573
          Padding.Left = 10
          Padding.Top = 9
          Padding.Right = 10
          Padding.Bottom = 9
          ParentBackground = False
          TabOrder = 5
          object EdtPesquisaTabela: TSearchBox
            Left = 10
            Top = 9
            Width = 394
            Height = 23
            Align = alClient
            TabOrder = 0
            OnChange = EdtPesquisaTabelaChange
            ExplicitHeight = 21
          end
        end
        object Panel4: TPanel
          Left = 10
          Top = 51
          Width = 414
          Height = 16
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Pesquisa por nome da tabela'
          Color = 7036495
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentBackground = False
          ParentFont = False
          TabOrder = 6
        end
      end
    end
    object panTopo: TPanel
      Left = 10
      Top = 0
      Width = 1035
      Height = 49
      Align = alTop
      BevelOuter = bvNone
      Color = 4866358
      ParentBackground = False
      TabOrder = 2
      object lblProjeto: TLabel
        Left = 10
        Top = 13
        Width = 306
        Height = 19
        Caption = 'Gerador de Classes para o PortalORM'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 585
    Width = 1055
    Height = 29
    Align = alBottom
    ButtonHeight = 27
    ButtonWidth = 137
    Caption = 'ToolBar1'
    DrawingStyle = dsGradient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    GradientEndColor = 7036495
    GradientStartColor = 10852231
    ParentFont = False
    ShowCaptions = True
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = actSair
      AutoSize = True
    end
    object ToolButton2: TToolButton
      Left = 88
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object ToolButton3: TToolButton
      Left = 96
      Top = 0
      Action = actGerarClasse
    end
    object ToolButton4: TToolButton
      Left = 233
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 2
      Style = tbsSeparator
    end
  end
  object DSTabelas: TDataSource
    DataSet = FDMetaInfoTabelas
    OnDataChange = DSTabelasDataChange
    Left = 228
    Top = 249
  end
  object DSCampos: TDataSource
    DataSet = FDMetaInfoCampos
    OnDataChange = DSCamposDataChange
    Left = 220
    Top = 401
  end
  object FDMetaInfoTabelas: TFDMetaInfoQuery
    Connection = FDConnection1
    Left = 144
    Top = 248
    object FDMetaInfoTabelasTABLE_NAME: TWideStringField
      FieldName = 'TABLE_NAME'
      ReadOnly = True
      Size = 128
    end
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'User_Name=sysdba'
      'Password=masterkey'
      'Database=D:\PROJETOS\Mercados\Dados\PRINCIPAL.FDB'
      'DriverID=FB')
    LoginPrompt = False
    Left = 64
    Top = 176
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 64
    Top = 128
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 256
    Top = 152
  end
  object FDMetaInfoCampos: TFDMetaInfoQuery
    Connection = FDConnection1
    MetaInfoKind = mkTableFields
    ObjectName = 'VEN_EST'
    Left = 136
    Top = 400
    object FDMetaInfoCamposRECNO: TIntegerField
      FieldName = 'RECNO'
      ReadOnly = True
    end
    object FDMetaInfoCamposCATALOG_NAME: TWideStringField
      FieldName = 'CATALOG_NAME'
      ReadOnly = True
      Size = 128
    end
    object FDMetaInfoCamposSCHEMA_NAME: TWideStringField
      FieldName = 'SCHEMA_NAME'
      ReadOnly = True
      Size = 128
    end
    object FDMetaInfoCamposTABLE_NAME: TWideStringField
      FieldName = 'TABLE_NAME'
      ReadOnly = True
      Size = 128
    end
    object FDMetaInfoCamposCOLUMN_NAME: TWideStringField
      FieldName = 'COLUMN_NAME'
      ReadOnly = True
      Size = 128
    end
    object FDMetaInfoCamposCOLUMN_POSITION: TIntegerField
      FieldName = 'COLUMN_POSITION'
      ReadOnly = True
    end
    object FDMetaInfoCamposCOLUMN_DATATYPE: TIntegerField
      FieldName = 'COLUMN_DATATYPE'
      ReadOnly = True
    end
    object FDMetaInfoCamposCOLUMN_TYPENAME: TWideStringField
      FieldName = 'COLUMN_TYPENAME'
      ReadOnly = True
      Size = 128
    end
    object FDMetaInfoCamposCOLUMN_ATTRIBUTES: TLongWordField
      FieldName = 'COLUMN_ATTRIBUTES'
      ReadOnly = True
    end
    object FDMetaInfoCamposCOLUMN_PRECISION: TIntegerField
      FieldName = 'COLUMN_PRECISION'
      ReadOnly = True
    end
    object FDMetaInfoCamposCOLUMN_SCALE: TIntegerField
      FieldName = 'COLUMN_SCALE'
      ReadOnly = True
    end
    object FDMetaInfoCamposCOLUMN_LENGTH: TIntegerField
      FieldName = 'COLUMN_LENGTH'
      ReadOnly = True
    end
  end
  object ActionList1: TActionList
    Left = 520
    Top = 312
    object actSair: TAction
      Caption = 'ESC - Sair'
      ShortCut = 27
      OnExecute = actSairExecute
    end
    object actGerarClasse: TAction
      Caption = 'F2 - Gerar Classe'
      ShortCut = 113
      OnExecute = actGerarClasseExecute
    end
  end
  object TimerPesquisa: TTimer
    Enabled = False
    Interval = 300
    OnTimer = TimerPesquisaTimer
    Left = 312
    Top = 128
  end
  object FDMetaInfoChavePrimaria: TFDMetaInfoQuery
    Connection = FDConnection1
    MetaInfoKind = mkPrimaryKeyFields
    BaseObjectName = 'PRODUTOS'
    ObjectName = 'PRODUTOS'
    Left = 136
    Top = 464
    object FDMetaInfoChavePrimariaRECNO: TIntegerField
      FieldName = 'RECNO'
      ReadOnly = True
    end
    object FDMetaInfoChavePrimariaCATALOG_NAME: TWideStringField
      FieldName = 'CATALOG_NAME'
      ReadOnly = True
      Size = 128
    end
    object FDMetaInfoChavePrimariaSCHEMA_NAME: TWideStringField
      FieldName = 'SCHEMA_NAME'
      ReadOnly = True
      Size = 128
    end
    object FDMetaInfoChavePrimariaTABLE_NAME: TWideStringField
      FieldName = 'TABLE_NAME'
      ReadOnly = True
      Size = 128
    end
    object FDMetaInfoChavePrimariaINDEX_NAME: TWideStringField
      FieldName = 'INDEX_NAME'
      ReadOnly = True
      Size = 128
    end
    object FDMetaInfoChavePrimariaCOLUMN_NAME: TWideStringField
      FieldName = 'COLUMN_NAME'
      ReadOnly = True
      Size = 128
    end
    object FDMetaInfoChavePrimariaCOLUMN_POSITION: TIntegerField
      FieldName = 'COLUMN_POSITION'
      ReadOnly = True
    end
    object FDMetaInfoChavePrimariaSORT_ORDER: TWideStringField
      FieldName = 'SORT_ORDER'
      ReadOnly = True
      Size = 128
    end
    object FDMetaInfoChavePrimariaFILTER: TWideStringField
      FieldName = 'FILTER'
      ReadOnly = True
      Size = 128
    end
  end
  object FileSaveDialog1: TFileSaveDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 665
    Top = 222
  end
end

unit SimulationInputUtils;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.Math,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    PluginCore,
    F8GLUtils;

const
    EDIT_MODE_MESSAGE_ACTION        = 'Ctrl�L�[���������܂܃}�E�X�𓮂����ĕύX�͈͂�I��';
    EDIT_MODE_MESSAGE_FINISH        = 'Esc�L�[�ŏI��';
    EDIT_WQ_MESSAGE_ACTION          = '�`���͈͓����N���b�N���đҋ@���ǉ�';
    SELECT_MODE_MESSAGE_ACTION      = 'Ctrl�L�[���������܂܃}�E�X�𓮂����đҋ@��`���͈͂�I��';
    SELECT_MODE_MESSAGE_CANCEL      = 'Esc�L�[�Œ��f';
    SELECT_EXIT_NODE_MESSAGE_ACTION = '�ҋ@���ǉ�����m�[�h���N���b�N���đI��';
    SELECT_POP_POINT_MESSAGE_ACTION = '�����n�_���N���b�N���đI��';
    SELECT_OUT_POINT_MESSAGE_ACTION = '�ړI�n���N���b�N���đI��';
    CHANGE_POINT_MESSAGE_ACTION     = '�ύX��̒n�_���N���b�N���đI��';
    SELECT_TERMINAL_MESSAGE_ACTION  = '�m�[�h���N���b�N���đI��';
    DEFAULT_POINT_NAME              = '�o�H';
    DEFAULT_POP_POINT_NAME          = '�����n�_';
    DEFAULT_OUT_POINT_NAME          = '�ړI�n';

    SELECT_DISPERSION_POINT_MESSAGE_ACTION        = '��n�_���N���b�N���đI��';
    SELECT_DISPERSION_POPOUT_POINT_MESSAGE_ACTION = '�C�ӂ̔���/�ޏo�n�_���N���b�N���đI���i�����I���j';
    CHANGE_DISPERSION_POINT_MESSAGE_ACTION        = '�ύX��̊�n�_���N���b�N���đI��';

    SAMPLE_PEDESTRIAN_CATEGORY_1 = 'Non handicapped adult';
    SAMPLE_PEDESTRIAN_CATEGORY_2 = 'Child or Old person';
    SAMPLE_PEDESTRIAN_CATEGORY_3 = 'Crutch';
    SAMPLE_PEDESTRIAN_CATEGORY_4 = 'Wheel chair';

    NAN_POINT : GLPointType = (NaN, NaN, NaN, NaN);

    MAXIMUM_SIGNAL_INTERVAL = 9999999;
    MOVING_LOG_INTERVAL     = 0.1;
    DEFAULT_ADJUST_RATE     = 1.0;

type
    TGetUpdatedtime  = function : TDateTime of Object;
    TRenderDataArray = array [0..1] of GLPointType;
    TPathNumArray    = array [0..1] of integer;
    TNodeNumArray    = array [0..1] of integer;
    TPopDetailArray  = array [0..23] of integer;

    PopOutPointType = (
        _None,
        _PopPoint,
        _TermOrGate,
        _OutPoint
        );

    PopRuleType = (
        _PopbyInterval,
        _PopbySchedule,
        _PopPerHour
        );

    OutRuleType = (
        _NoRule,
        _OutbyInterval,
        _OutbySchedule
        );

    PointAttrType = (
        _Normal,
        _BusTerminal,
        _TicketGate
        );

implementation

end.

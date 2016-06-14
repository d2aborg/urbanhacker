function share(link) {
    var width = 520;
    var height = 350;
    var top = (screen.height / 2) - (height / 2);
    var left = (screen.width / 2) - (width / 2);

    var url = $(link).attr('href');
    var target = $(link).attr('target');

    window.open(url, target, 'top=' + top + ',left=' + left + ',toolbar=0,status=0,width=' + width + ',height=' + height);

    return false;
}

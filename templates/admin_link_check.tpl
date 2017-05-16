{% extends "admin_base.tpl" %}

{% block title %}{_ External Link Check _}{% endblock %}

{% block content %}

    <table class="table table-striped">
        <thead>
            <tr>
                <th>Address</th>
                <th>Status</th>
                <th>Last Success</th>
                <th></th>
            </tr>
        </thead>
        {% with q.offset|default:0 as offset  %}
            {% for link in m.link.problems[offset] %}
            <tr>
                {% with link.url as link %}
                    <td>
                        <pre><a href="{{ link }}">{{ link|truncate:80 }}</a></pre>
                    </td>
                {% endwith %}
                <td title="{{ link.status_tooltip }}">{{ link.friendly_status }}</td>
                <td title="Last checked: {{ link.last_check }}">{{ link.last_success }}</td>
                <td>
                    {% with link.rsc_id as rsc_id %}
                        <span class="pull-right buttons">
                            {% with "check-" ++ link.id as btn_id %}
                                <button id="{{ btn_id }}" class="btn btn-default btn-xs">{_ check now _}</button>
                                {% wire id=btn_id postback={check url=link.url} delegate="mod_link_check" %}
                            {% endwith %}
                            <a href="{{ rsc_id.id.page_url }}" class="btn btn-default btn-xs">{_ view page _}</a>
                            <a href="{% url admin_edit_rsc id=rsc_id.id %}" class="btn btn-default btn-xs">{_ edit resource _}</a>
                        </span>
                    {% endwith %}
                </td>
            </tr>
            {% endfor %}
        </table>
        <button id="{{ #btn_more }}" class="btn btn-default btn-lg">{_ More _}</button>
        {% with "?offset=" ++ (offset + 15) as qoffset %}
            {% wire id=#btn_more action={redirect location=qoffset} %}
        {% endwith %}
    {% endwith %}
{% endblock %}

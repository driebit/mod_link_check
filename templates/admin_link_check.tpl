{% extends "admin_base.tpl" %}

{% block title %}{_ External Link Check _}{% endblock %}

{% block content %}

    {# lib "css/admin_link_check.css" #}

    <table class="table table-striped">
        <thead>
            <tr>
                <th>Address</th>
                <th>Status</th>
                <th>Last Success</th>
                <th></th>
            </tr>
        </thead>
        {% for link in m.link.problem %}
        <tr>
            {% with link.url as link %}
                <td><a href="{{ link }}">{{ link|truncate:80 }}</a></td>
            {% endwith %}
            <td title="{{ link.status_tooltip }}">{{ link.friendly_status }}</td>
            <td title="Last checked: {{ link.last_check }}">{{ link.last_success }}</td>
            <td>
                {% with link.rsc_id as rsc_id %}
                    <span class="pull-right buttons">
                        <a href="{{ rsc_id.id.page_url }}" class="btn btn-default btn-xs">{_ view _}</a>
                        <a href="{% url admin_edit_rsc id=rsc_id.id %}" class="btn btn-default btn-xs">{_ edit _}</a>
                    </span>
                {% endwith %}
            </td>
        </tr>
        {% endfor %}
    </table>
{% endblock %}

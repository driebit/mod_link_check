{% extends "admin_base.tpl" %}

{% block title %}{_ External Link Check _}{% endblock %}

{% block content %}

    {# lib "css/admin_link_check.css" #}

    <table class="table table-striped">
        <thead>
            <tr>
                <th>Address</th>
                <th>Last Check</th>
                <th>Last Success</th>
                <th>Last Status</th>
                <th>Invalid</th>
                <th>Error Reason</th>
                <th></th>
            </tr>
        </thead>
        {% for link in m.link.problem %}
        <tr>
            <td>{{ link.url|truncate:50 }}</td>
            <td>{{ link.last_check }}</td>
            <td>{{ link.last_success }}</td>
            <td>{{ link.last_status }}</td>
            <td>{{ link.invalid }}</td>
            <td>{{ link.error_reason }}</td>
            <td>
                {% with link.rsc_id as rsc_id %}
                    <span class="pull-right buttons">
                        <a href="{{ rsc_id.id.page_url }}" class="btn btn-default btn-xs">{_ view _}</a>
                        <a href="{% url admin_edit_rsc id=rsc_id.id.page_url %}" class="btn btn-default btn-xs">{_ edit _}</a>
                    </span>
                {% endwith %}
            </td>
        </tr>
        {% endfor %}
    </table>
{% endblock %}
